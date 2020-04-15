;;; solaire-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "solaire-mode" "solaire-mode.el" (0 0 0 0))
;;; Generated autoloads from solaire-mode.el

(autoload 'solaire-mode "solaire-mode" "\
Make source buffers grossly incandescent by remapping common faces (see
`solaire-mode-remap-alist') to their solaire-mode variants.

\(fn &optional ARG)" t nil)

(defvar solaire-global-mode nil "\
Non-nil if Solaire-Global mode is enabled.
See the `solaire-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `solaire-global-mode'.")

(custom-autoload 'solaire-global-mode "solaire-mode" nil)

(autoload 'solaire-global-mode "solaire-mode" "\
Toggle Solaire mode in all buffers.
With prefix ARG, enable Solaire-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Solaire mode is enabled in all buffers where
`turn-on-solaire-mode' would do it.
See `solaire-mode' for more information on Solaire mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-solaire-mode "solaire-mode" "\
Conditionally enable `solaire-mode' in the current buffer.

Does nothing if the current buffer doesn't satisfy the function in
`solaire-mode-real-buffer-fn'.

\(fn)" t nil)

(autoload 'turn-off-solaire-mode "solaire-mode" "\
Disable `solaire-mode' in the current buffer.

\(fn)" t nil)

(autoload 'solaire-mode-in-minibuffer "solaire-mode" "\
Highlight the minibuffer whenever it is active.

\(fn)" nil nil)

(autoload 'solaire-mode-reset "solaire-mode" "\
Reset all buffers with `solaire-mode' enabled.

The purpose for this is to reset faces that cannot be buffer-local such as the
fringe, which can be changed by loading a new theme or opening an Emacs client
frame with a different display (via emacsclient).

\(fn &rest _)" t nil)

(autoload 'solaire-mode-swap-bg "solaire-mode" "\
Swap the backgrounds of the following faces:

+ `default' <-> `solaire-default-face'
+ `hl-line' <-> `solaire-hl-line-face'
+ `org-hide' <-> `solaire-org-hide-face'

This is necessary for themes in the doom-themes package.

\(fn)" nil nil)

(autoload 'solaire-mode-restore-persp-mode-buffers "solaire-mode" "\
Restore `solaire-mode' in buffers when `persp-mode' loads a session.

\(fn &rest _)" nil nil)

(advice-add #'load-theme :after (lambda (&rest _) (setq solaire-mode--pending-bg-swap (bound-and-true-p solaire-mode-auto-swap-bg))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "solaire-mode" '("solaire-mode-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; solaire-mode-autoloads.el ends here
