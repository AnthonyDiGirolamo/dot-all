;;; ibuffer-vc.el --- Group ibuffer's list by VC project, or show VC status  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2011-2014 Steve Purcell
;;
;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1") (seq "2"))
;; URL: https://github.com/purcell/ibuffer-vc
;; Package-Version: 20241106.1518
;; Package-Revision: 890c692da934
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Adds functionality to ibuffer for grouping buffers by their parent
;; vc root directory, and for displaying and/or sorting by the vc
;; status of listed files.
;;
;;; Use:
;;
;; To group buffers by vc parent dir:
;;
;;   M-x ibuffer-vc-set-filter-groups-by-vc-root
;;
;; or, make this the default:
;;
;;   (add-hook 'ibuffer-hook
;;     (lambda ()
;;       (ibuffer-vc-set-filter-groups-by-vc-root)
;;       (unless (eq ibuffer-sorting-mode 'alphabetic)
;;         (ibuffer-do-sort-by-alphabetic))))
;;
;; Alternatively, use `ibuffer-vc-generate-filter-groups-by-vc-root'
;; to programmatically obtain a list of filter groups that you can
;; combine with your own custom groups.
;;
;; To include vc status info in the ibuffer list, add either
;; vc-status-mini or vc-status to `ibuffer-formats':
;;
;; (setq ibuffer-formats
;;       '((mark modified read-only vc-status-mini " "
;;               (name 18 18 :left :elide)
;;               " "
;;               (size 9 -1 :right)
;;               " "
;;               (mode 16 16 :left :elide)
;;               " "
;;               (vc-status 16 16 :left)
;;               " "
;;               vc-relative-file)))
;;
;; To sort by vc status, use `ibuffer-do-sort-by-vc-status', which can
;; also be selected by repeatedly executing
;; `ibuffer-toggle-sorting-mode' (bound to "," by default).
;;
;;; Code:

;; requires

(require 'ibuffer)
(require 'ibuf-ext)
(require 'vc-hooks)
(require 'seq)


(defgroup ibuffer-vc nil
  "Group ibuffer entries according to their version control status."
  :prefix "ibuffer-vc-"
  :group 'convenience)

(defcustom ibuffer-vc-skip-if-remote t
  "If non-nil, don't query the VC status of remote files."
  :type 'boolean
  :group 'ibuffer-vc)

(defcustom ibuffer-vc-include-function 'identity
  "A function which tells whether a given file should be grouped.

The function is passed a filename, and should return non-nil if the file
is to be grouped.

This option can be used to exclude certain files from the grouping mechanism."
  :type 'function
  :group 'ibuffer-vc)

(defcustom ibuffer-vc-buffer-file-name-function 'ibuffer-vc-buffer-file-truename
  "Function which tells the file name associated with a buffer.

The function is passed a buffer, and should return the file name
to associate with that buffer.

This option can be configured along with
`ibuffer-vc-include-function' to include or exclude certain
buffers from the grouping mechanism."
  :type 'function
  :group 'ibuffer-vc)

;;; Group and filter ibuffer entries by parent vc directory

(defun ibuffer-vc-buffer-file-truename (buf)
  "Return the truename of the file associated with BUF.

The file associated with BUF is the one that BUF is visiting,
whose file name is stored in the buffer-local variable
`buffer-file-name'.

If that is nil, but BUF sets the variable
`list-buffers-directory' (for example, Dired and Magit buffers),
then consider that directory to be the file associated with the
buffer.

If neither of those is set for BUF, return nil."
  (with-current-buffer buf
    (when-let ((file-name (or buffer-file-name
                              list-buffers-directory)))
      (file-truename file-name))))

(defun ibuffer-vc--include-file-p (file)
  "Return t iff FILE should be included in ibuffer-vc's filtering."
  (and file
       (or (null ibuffer-vc-skip-if-remote)
           (not (file-remote-p file)))
       (funcall ibuffer-vc-include-function file)))

(defun ibuffer-vc--deduce-backend (file)
  "Return the vc backend for FILE, or nil if not under VC supervision."
  (if (fboundp 'vc-responsible-backend)
      (ignore-errors (vc-responsible-backend file))
    (or (vc-backend file)
        (seq-filter (lambda (b) (vc-call-backend b 'responsible-p file)) vc-handled-backends))))

(defun ibuffer-vc-root (buf)
  "Return a cons cell (backend-name . root-dir) for BUF.
If the file is not under version control, nil is returned instead."
  (let ((file-name (funcall ibuffer-vc-buffer-file-name-function buf)))
    (when (ibuffer-vc--include-file-p file-name)
      (when-let ((backend (ibuffer-vc--deduce-backend file-name)))
        (let* ((root-fn-name (intern (format "vc-%s-root" (downcase (symbol-name backend)))))
               (root-dir
                (cond
                 ((fboundp root-fn-name) (funcall root-fn-name file-name)) ; git, svn, hg, bzr (at least)
                 ((and (fboundp 'vc-darcs-find-root)                       ; vc-darcs is an external package
                       (memq backend '(darcs DARCS)))
                  (vc-darcs-find-root file-name))
                 ((memq backend '(cvs CVS)) (vc-find-root file-name "CVS"))
                 ((memq backend '(rcs RCS)) (or (vc-find-root file-name "RCS")
                                                (concat file-name ",v")))
                 ((memq backend '(sccs SCCS)) (or (vc-find-root file-name "SCCS")
                                                  (concat "s." file-name)))
                 ((memq backend '(src SRC)) (or (vc-find-root file-name ".src")
                                                (concat file-name ",v")))
                 (t (error "ibuffer-vc: don't know how to find root for vc backend '%s' - please submit a bug report or patch" backend)))))
          (cons backend root-dir))))))

(defun ibuffer-vc-read-filter ()
  "Read a cons cell of (backend-name . root-dir)."
  (cons (car (read-from-string
              (completing-read "VC backend: " vc-handled-backends nil t)))
        (read-directory-name "Root directory: " nil nil t)))

(define-ibuffer-filter vc-root
    "Toggle current view to buffers with vc root dir QUALIFIER."
  (:description "vc root dir"
                :reader (ibuffer-vc-read-filter))
  (when-let ((it (ibuffer-vc-root buf)))
    (equal qualifier it)))

;;;###autoload
(defun ibuffer-vc-generate-filter-groups-by-vc-root ()
  "Create a set of ibuffer filter groups based on the vc root dirs of buffers."
  (let ((roots (seq-uniq
                (delq nil (mapcar 'ibuffer-vc-root (buffer-list))))))
    (mapcar (lambda (vc-root)
              (cons (format "%s: %s" (car vc-root) (cdr vc-root))
                    `((vc-root . ,vc-root))))
            roots)))

;;;###autoload
(defun ibuffer-vc-set-filter-groups-by-vc-root ()
  "Set the current filter groups to filter by vc root dir."
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root))
  (message "ibuffer-vc: groups set")
  (when-let ((ibuf (get-buffer "*Ibuffer*")))
    (with-current-buffer ibuf
      (pop-to-buffer ibuf)
      (ibuffer-update nil t))))


;;; Display vc status info in the ibuffer list

(defun ibuffer-vc--state (file)
  "Return the `vc-state' for FILE, or nil if unregistered."
  (ignore-errors (vc-state file)))

(defun ibuffer-vc--status-string ()
  "Return a short string to represent the current buffer's status."
  (when (and buffer-file-name (ibuffer-vc--include-file-p buffer-file-name))
    (let ((state (ibuffer-vc--state buffer-file-name)))
      (if state
          (symbol-name state)
        "-"))))

;;;###autoload (autoload 'ibuffer-make-column-vc-status "ibuffer-vc")
(define-ibuffer-column vc-status
  (:name "VC status")
  (ibuffer-vc--status-string))

;;;###autoload (autoload 'ibuffer-make-column-vc-relative-file "ibuffer-vc")
(define-ibuffer-column vc-relative-file
  (:name "Filename")
  (if buffer-file-name
      (let ((root (cdr (ibuffer-vc-root buffer))))
        (if root
            (file-relative-name buffer-file-name root)
          (abbreviate-file-name buffer-file-name)))
    ""))

;;;###autoload (autoload 'ibuffer-make-column-vc-status-mini "ibuffer-vc")
(define-ibuffer-column vc-status-mini
  (:name "V")
  (if (and buffer-file-name (ibuffer-vc--include-file-p buffer-file-name))
      (let ((state (ibuffer-vc--state buffer-file-name)))
        (cond
         ((eq 'added state) "A")
         ((eq 'removed state) "D")
         ((eq 'up-to-date state) "=")
         ((eq 'edited state) "*")
         ((eq 'needs-update state) "O")
         ((memq state '(conflict needs-merge unlocked-changes)) "!")
         ((eq 'ignored state) "I")
         ((memq state '(() unregistered missing)) "?")))
    " "))

;;;###autoload (autoload 'ibuffer-do-sort-by-vc-status "ibuffer-vc")
(define-ibuffer-sorter vc-status
  "Sort the buffers by their vc status."
  (:description "vc status")
  (let ((file1 (with-current-buffer (car a)
                 buffer-file-name))
        (file2 (with-current-buffer (car b)
                 buffer-file-name)))
    (if (and file1 file2)
        (string-lessp (with-current-buffer (car a)
                        (ibuffer-vc--status-string))
                      (with-current-buffer (car b)
                        (ibuffer-vc--status-string)))
      (not (null file1)))))


(provide 'ibuffer-vc)
;;; ibuffer-vc.el ends here
