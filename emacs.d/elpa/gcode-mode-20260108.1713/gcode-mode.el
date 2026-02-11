;;; gcode-mode.el --- Simple G-Code major mode  -*- lexical-binding: t -*-

;; Author: Yuri D'Elia <wavexx@thregr.org>
;; Package-Version: 20260108.1713
;; Package-Revision: a0423aab9aba
;; URL: https://gitlab.com/wavexx/gcode-mode.el
;; Package-Requires: ((emacs "24.4"))
;; Keywords: gcode, languages, highlight, syntax

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; `gcode-mode' performs basic syntax highlighting on G-Code files
;; (mostly aimed at 3D printers), also providing optional instruction
;; lookup with ElDoc.
;;
;; Once installed, all gcode files automatically open in this mode.
;; To also automatically enable ElDoc in G-Code files use:
;;
;; (add-hook 'gcode-mode-hook 'eldoc-mode)
;;
;; ElDoc will provide brief descriptions of the current instruction at
;; point. Embedded documentation is provided thanks to both the RepRap
;; Wiki[0] and the Marlin Documentation[1] projects.
;;
;; [0] https://reprap.org/wiki/G-code
;; [1] https://github.com/MarlinFirmware/MarlinDocumentation/

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gco\\(?:de\\)?\\'" . gcode-mode))


;; Customizable faces
(defgroup gcode-mode-faces nil
  "Faces used in `gcode-mode'."
  :group 'faces)

(defface gcode-mode-line-number-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used for line numbers."
  :group 'gcode-mode-faces)

(defface gcode-mode-checksum-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used for checksums."
  :group 'gcode-mode-faces)

(defface gcode-mode-gcode-face
  '((t :inherit font-lock-builtin-face))
  "Face used for main G-Code (move) instructions."
  :group 'gcode-mode-faces)

(defface gcode-mode-mcode-face
  '((t :inherit font-lock-keyword-face))
  "Face used for main M-Code (machine) instructions."
  :group 'gcode-mode-faces)

(defface gcode-mode-dcode-face
  '((t :inherit font-lock-warning-face))
  "Face used for main D-Code (debug) instructions."
  :group 'gcode-mode-faces)

(defface gcode-mode-tcode-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used for main T/P (tool) instructions."
  :group 'gcode-mode-faces)

(defface gcode-mode-subtype-face
  '((t :weight bold))
  "Face used for highlighting subtypes of the form \"GX.Y\"."
  :group 'gcode-mode-faces)

(defface gcode-mode-argument-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for G-Code argument names."
  :group 'gcode-mode-faces)


;; Lazy documentation table generation
(declare-function gcode-mode--doc-entries "gcode-mode-doc")

(defvar gcode-mode--doc-hash nil
  "G-Code documentation table (lazy-built by `gcode-mode--doc-build').")

(defun gcode-mode--doc-build ()
  "Populate G-Code documentation hash table."
  (unless gcode-mode--doc-hash
    (require 'gcode-mode-doc)

    ;; populate the hash table
    (let ((hash (make-hash-table :test 'equal)))
      (dolist (entry (gcode-mode--doc-entries))
	(let ((code (car entry))
	      (def (cdr entry)))
	  (let ((def-list (gethash code hash)))
	    (push def def-list)
	    (puthash code def-list hash))))
      (setq gcode-mode--doc-hash hash))

    ;; unload gcode-mode-doc
    (unload-feature 'gcode-mode-doc)))


;; Documentation formatting
(defun gcode-mode--params-list (params)
  (when params
    (cons (car params) (gcode-mode--params-list (cddr params)))))

(defun gcode-mode--doc-format (_instr entries param)
  "Format the retrieved documentation entry/es for display."
  (let ((entry (car entries))) ; TODO: display/select multiple candidates
    (let* ((title (car entry))
	   (params (cdr entry))
	   (param-desc (when param (plist-get params (string-to-char param))))
	   (param-list (unless param-desc (gcode-mode--params-list params))))
      (let ((docstring title))
	(when param-list
	  (setq docstring
		(concat docstring " ["
			(propertize
			 (concat param-list)
			 'face 'gcode-mode-argument-face)
			"]")))
	(when param-desc
	  (setq docstring
		(concat docstring " "
			(propertize param 'face 'gcode-mode-argument-face)
			": " param-desc)))
	docstring))))

(defun gcode-mode--instr-face (instr)
  "Return the appropriate face for the current G-Code instruction INSTR."
  (let ((code (string-to-char instr)))
    (cond ((equal ?M code) 'gcode-mode-mcode-face)
	  ((equal ?D code) 'gcode-mode-dcode-face)
	  ((equal ?T code) 'gcode-mode-tcode-face)
	  ((equal ?P code) 'gcode-mode-tcode-face)
	  (t 'gcode-mode-gcode-face))))

(defun gcode-mode--eldoc-core ()
  "Lookup current G-Code instruction at point."
  (save-excursion
    (let ((pos (point)))
      (beginning-of-line)
      (when (looking-at "^\\s-*\\(?:N[0-9]+\\s-+\\)?\\([GMTDP]-?\\)0*\\([0-9]+\\)\\(\\.[0-9]*\\)?\\_>")
	(let* ((args-pos (match-end 0))
	       (code (concat (match-string-no-properties 1) (match-string-no-properties 2)))
	       (subtype (replace-regexp-in-string "0+$" "" (or (match-string-no-properties 3) "")))
	       (instr (concat code subtype))
	       (entries (gethash instr gcode-mode--doc-hash)))
	  (unless entries
	    ;; attempt to lookup main code if instr doesn't exist
	    (setq entries (gethash code gcode-mode--doc-hash)
		  instr code))
	  (when entries
	    (let ((face (gcode-mode--instr-face code))
		  (param))
	      ;; fetch current parameter
	      (goto-char pos)
	      (when (and (> pos args-pos)
			 (not (nth 4 (syntax-ppss))))
		(skip-syntax-backward "^ ")
		(when (looking-at "\\([A-Z]\\)\\S-*")
		  (setq param (match-string-no-properties 1))))
	      ;; return final documentation
	      (list (gcode-mode--doc-format instr entries param)
		    :thing instr :face face))))))))

(defun gcode-mode--eldoc-compat ()
  "Lookup current G-Code instruction at point for old versions of eldoc."
  (gcode-mode--doc-build)
  (let ((ret (gcode-mode--eldoc-core)))
    (when ret
      (let* ((doc (car ret))
	     (attrs (cdr ret))
	     (thing (plist-get attrs :thing))
	     (face (plist-get attrs :face)))
	(concat (propertize thing 'face face) ": " doc)))))

(defun gcode-mode--eldoc-function (callback)
  "Lookup current G-Code instruction at point and call CALLBACK."
  (gcode-mode--doc-build) ; TODO: this can/should be async
  (when callback
    (let ((ret (gcode-mode--eldoc-core)))
      (when ret
	(apply callback ret)))))


;; Main code

;;;###autoload
(define-derived-mode gcode-mode prog-mode "G-Code"
  "Major mode for G-Code instructions."

  ;; handle comments
  (setq-local comment-start "; "
              comment-end "")

  (font-lock-add-keywords
   nil
   '(;; comments
     (";[^\n]*" . 'font-lock-comment-face)
     ;; line numbers
     ("^\\s-*\\(N[0-9]+\\)\\_>" (1 'gcode-mode-line-number-face))
     ;; checksums
     ("\\(\\*[0-9]+\\)\\s-*\\(?:$\\|\\s<\\)" (1 'gcode-mode-checksum-face))
     ;; instructions + subtype
     ("^\\s-*\\(?:N[0-9]+\\s-+\\)?\\([GMTDP]-?[0-9]+\\(\\.[0-9]*\\)?\\)\\_>"
      (1 (gcode-mode--instr-face (match-string-no-properties 1)))
      (2 'gcode-mode-subtype-face prepend t)
      ;; arguments
      ("\\_<[A-Z]" nil nil (0 'gcode-mode-argument-face)))))

  ;; eldoc
  (if (boundp 'eldoc-documentation-functions) ; Emacs>=28
      (add-hook 'eldoc-documentation-functions #'gcode-mode--eldoc-function nil t)
    (setq-local eldoc-documentation-function #'gcode-mode--eldoc-compat)))

(provide 'gcode-mode)

;;; gcode-mode.el ends here
