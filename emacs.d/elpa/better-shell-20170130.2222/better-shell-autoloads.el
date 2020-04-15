;;; better-shell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "better-shell" "better-shell.el" (22676 53128
;;;;;;  455454 774000))
;;; Generated autoloads from better-shell.el

(autoload 'better-shell-remote-open "better-shell" "\
Prompt for a remote host to connect to, and open a shell there.

\(fn)" t nil)

(autoload 'better-shell-shell "better-shell" "\
Pop to an appropriate shell.
Cycle through all the shells, most recently used first.  When
called with a prefix ARG, finds or creates a shell in the current
directory.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; better-shell-autoloads.el ends here
