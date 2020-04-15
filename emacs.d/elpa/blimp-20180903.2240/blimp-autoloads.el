;;; blimp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "blimp" "blimp.el" (0 0 0 0))
;;; Generated autoloads from blimp.el

(autoload 'blimp-mode "blimp" "\
Toggle Blimp mode.

\(fn &optional ARG)" t nil)

(autoload 'blimp-toggle-prefix "blimp" "\
Toggle the command prefix.
If ARG is positive, use + as command prefix.
If ARG is negative, use - as command prefix.
Otherwise toggle between command prefixes.

\(fn &optional ARG)" t nil)

(autoload 'blimp-clear-command-stack "blimp" "\
Remove all unexecuted commands.

\(fn)" t nil)

(autoload 'blimp-execute-command-stack "blimp" "\
Execute all unexecuted commands.
Also removes all unexecuted commands after executing them.

\(fn)" t nil)

(autoload 'blimp-interface "blimp" "\
Prompt user for arguments of COMMAND if any and add to command stack.
If COMMAND is nil, prompt user for which command should be executed.

\(fn &optional COMMAND)" t nil)

(autoload 'blimp-interface-execute "blimp" "\
Prompt user for arguments of COMMAND if any and add to command stack.
If COMMAND is nil, prompt user for which command should be executed.
COMMAND will be executed instantly.

\(fn &optional COMMAND)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "blimp" '("blimp-")))

;;;***

;;;### (autoloads nil nil ("blimp-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; blimp-autoloads.el ends here
