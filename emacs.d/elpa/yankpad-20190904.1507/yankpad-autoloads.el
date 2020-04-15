;;; yankpad-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yankpad" "yankpad.el" (0 0 0 0))
;;; Generated autoloads from yankpad.el

(autoload 'yankpad-set-category "yankpad" "\
Change the yankpad category.

\(fn)" t nil)

(autoload 'yankpad-insert "yankpad" "\
Insert an entry from the yankpad.
Uses `yankpad-category', and prompts for it if it isn't set.

\(fn)" t nil)

(autoload 'yankpad-capture-snippet "yankpad" "\
`org-capture' a snippet to current `yankpad-category' (prompts if not set).

\(fn)" t nil)

(autoload 'yankpad-expand "yankpad" "\
Replace symbol at point with a snippet.
Only works if the symbol is found in the first matching group of
`yankpad-expand-keyword-regex'.

This function can be added to `hippie-expand-try-functions-list'.

\(fn &optional FIRST)" t nil)

(autoload 'yankpad-edit "yankpad" "\
Open the yankpad file for editing.

\(fn)" t nil)

(autoload 'yankpad-map "yankpad" "\
Create and execute a keymap out of the last tags of snippets in `yankpad-category'.

\(fn)" t nil)

(autoload 'company-yankpad "yankpad" "\
Company backend for yankpad.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yankpad" '("company-yankpad--name-or-key" "yankpad-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yankpad-autoloads.el ends here
