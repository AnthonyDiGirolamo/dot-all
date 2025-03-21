;;; lispyville-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from lispyville.el

(autoload 'lispyville-mode "lispyville" "\
A minor mode for integrating evil with lispy.

This is a minor mode.  If called interactively, toggle the `Lispyville
mode' mode.  If the prefix argument is positive, enable the mode, and if
it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `lispyville-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

\\{lispyville-mode-map}

(fn &optional ARG)" t)
(autoload 'lispyville-set-key-theme "lispyville" "\
Binds keys in lispyville-mode-map according to THEME.
When THEME is not given, `lispyville-key-theme' will be used instead.

(fn &optional THEME)")
(register-definition-prefixes "lispyville" '("lispyville-"))

;;; End of scraped data

(provide 'lispyville-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; lispyville-autoloads.el ends here
