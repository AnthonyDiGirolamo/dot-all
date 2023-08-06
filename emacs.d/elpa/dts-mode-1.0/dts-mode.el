;;; dts-mode.el --- Major mode for Device Tree source files  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2022  Free Software Foundation, Inc.

;; Version: 1.0
;; Author: Ben Gamari <ben@smart-cactus.org>
;; Package-Requires: ((emacs "24"))
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides basic editing support for DTS files (see
;; http://www.devicetree.org/), with the following features:
;;
;; - Font-lock highlighting
;; - SMIE-based automatic indentation

;;; News:

;; In Version 1.0:
;; - Always use SMIE
;; - Require Emacs≥24

;; In Version 0.1.1:
;; - Add SMIE-based indentation

;;; Code:

(defconst dts-re-ident "\\([[:alpha:]_][[:alnum:]_,-]*\\)")

(defvar dts-mode-font-lock-keywords
  `(
    ;; Names like `name: hi {`
    (,(concat dts-re-ident ":") 1 font-lock-variable-name-face)
    ;; Nodes
    (,(concat dts-re-ident "\\(@[[:xdigit:]]+\\)?[[:space:]]*{")
     (1 font-lock-type-face))
    ;; Assignments
    (,(concat dts-re-ident "[[:space:]]*=") 1 font-lock-variable-name-face)
    (,(concat dts-re-ident "[[:space:]]*;") 1 font-lock-variable-name-face)
    ;; References
    (,(concat "&" dts-re-ident) 1 font-lock-variable-name-face)
    )
  )

(defvar dts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?<  "(>" table)
    (modify-syntax-entry ?>  ")<" table)

    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?~  "." table)

    ;; _ and , are both symbol constituents.
    (modify-syntax-entry ?,  "_" table)
    (modify-syntax-entry ?_  "_" table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

;;;; Original manual indentation code.

(defun dts--calculate-indentation ()
  (declare (obsolete indent-according-to-mode "1.0"))
  (save-excursion
    (let ((end (point-at-eol))
          (cnt 0)
          (initial-point (point)))
      (goto-char 0)
      (while (re-search-forward "\\([{}]\\)" end t)
        (if (string= (match-string-no-properties 0) "{")
            (setq cnt (1+ cnt))
          (setq cnt (1- cnt))))
      ;; subtract one if the current line has an opening brace since we
      ;; shouldn't add the indentation level until the following line
      (goto-char initial-point)
      (beginning-of-line)
      (when (re-search-forward "{" (point-at-eol) t)
        (setq cnt (1- cnt)))
      cnt)))

(defun dts-indent-line ()
  "Old indentation algorithm."
  (declare (obsolete indent-according-to-mode "1.0"))
  (interactive)
  (let ((indent (with-no-warnings (dts--calculate-indentation))))
    (save-excursion
      (indent-line-to (* indent tab-width)))
    (when (or (bolp) (looking-back "^[[:space:]]+" (line-beginning-position)))
      (beginning-of-line-text))))

;;;; New SMIE-based indentation code.

;; Compatibility macro.
(defmacro dts--using-macro (name exp)
  (declare (indent 1) (debug (symbolp form)))
  (if (fboundp name)            ;If macro exists at compiler-time, just use it.
      exp
    `(when (fboundp ',name)            ;Else, check if it exists at run-time.
       (eval ',exp))))                 ;If it does, then run the code.

(require 'smie nil t)


(defconst dts-grammar
  ;; FIXME: The syntax-table gives symbol-constituent syntax to the comma,
  ;; but the comma is also used as a separator!
  (when (fboundp 'smie-prec2->grammar)
    (smie-prec2->grammar
     (smie-bnf->prec2
      '((id) (val ("<" val ">"))
        (exp ("{" exps "}")
             ;; The "foo,bar = toto" can be handled either by considering
             ;; "foo,bar" as a single token or as 3 tokens.
             ;; Currently I consider it as 3 tokens, so the LHS of "=" can't be
             ;; just `id' but has to be `vals'.
             (vals "=" vals))
        (exps (exp) (exps ";" exps))
        (vals  (val "," val)))
      '((assoc ";")) '((assoc ","))))))

(defun dts-indent-rules (kind token)
  (dts--using-macro pcase
    (pcase (cons kind token)
      (`(:elem . basic) tab-width)
      ;; (`(:elem . args) 0)
      (`(:list-intro . "")                ;FIXME: Not sure why we get "" here!
       ;; After < we either have a plain list of data, as in: "operating-points
       ;;  = <1008000 1400000 ...>" or we have sometimes "refs with args" as in
       ;;  "clocks = <&apb1_gates 6>;".
       (and (eq (char-before) ?<) (not (looking-at "&"))))
      (`(:before . "{") (smie-rule-parent))
      (`(:before . "<") (if (smie-rule-hanging-p) (smie-rule-parent)))
      (`(:after . "=") (dts-indent-rules :elem 'basic))
      )))

;;;; The major mode itself.

(defalias 'dts-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode dts-mode dts-parent-mode "Devicetree";DTS would be shorter!
  "Major mode for editing Device Tree source files."

  ;; Fonts
  (set (make-local-variable 'font-lock-defaults)
       '(dts-mode-font-lock-keywords nil nil nil nil))
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end)   " */")
  (set (make-local-variable 'comment-multi-line) t)

  ;; This is not specific to the DTS format, really, but DTS is mostly
  ;; used in the context of the Linux kernel (and U-boot loader) where
  ;; there's a strong preference to indent with TABs.
  (set (make-local-variable 'indent-tabs-mode) t)

  (dts--using-macro syntax-propertize-rules
    (set (make-local-variable 'syntax-propertize-function)
         (syntax-propertize-rules
          ("#include[ \t]+\\(<\\).*\\(>\\)" (1 "|") (2 "|"))
          ;; Treat things like /delete-property/ as a single identifier.
          ("\\(/\\)[a-z]+\\(/\\)" (1 "_") (2 "_")))))
  (smie-setup dts-grammar #'dts-indent-rules))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dtsi?\\'" . dts-mode))

(provide 'dts-mode)
;;; dts-mode.el ends here
