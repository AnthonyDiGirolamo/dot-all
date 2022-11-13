;;; amx-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "amx" "amx.el" (0 0 0 0))
;;; Generated autoloads from amx.el

(defvar amx-mode nil "\
Non-nil if Amx mode is enabled.
See the `amx-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `amx-mode'.")

(custom-autoload 'amx-mode "amx" nil)

(autoload 'amx-mode "amx" "\
Use ido completion for M-x

This is a minor mode.  If called interactively, toggle the `Amx
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='amx-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'amx "amx" "\
Read a command name and execute the command.

This is the main entry point for the Amx package, an alternative
to the normal \\[execute-extended-command] built into Emacs that
provides several extra features." t nil)

(autoload 'amx-major-mode-commands "amx" "\
Like `amx', but limited to commands that are relevant to the active major mode." t nil)

(autoload 'amx-initialize "amx" "\
Ensure that amx is properly initialized.

This function is normally idempotent, only having an effect the
first time it is called, so it is safe to call it at the
beginning of any function that expects amx to be initialized.
However, optional arg REINIT forces the initialization needs to
be re-run. Interactively, reinitialize when a prefix arg is
provided.

\(fn &optional REINIT)" t nil)

(register-definition-prefixes "amx" '("amx-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; amx-autoloads.el ends here
