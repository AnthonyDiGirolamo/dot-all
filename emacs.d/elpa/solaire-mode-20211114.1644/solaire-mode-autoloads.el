;;; solaire-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "solaire-mode" "solaire-mode.el" (0 0 0 0))
;;; Generated autoloads from solaire-mode.el

(defface solaire-default-face '((t :inherit default)) "\
Alternative version of the `default' face." :group 'solaire-mode)

(autoload 'solaire-mode "solaire-mode" "\
Make current buffer a different color so others can be grossly incandescent.

If called interactively, enable Solaire mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

Remaps faces in `solaire-mode-remap-alist', then runs `solaire-mode-hook', where
additional mode-specific fixes may live. Lastly, adjusts the fringes for the
current frame.

\(fn &optional ARG)" t nil)

(put 'solaire-global-mode 'globalized-minor-mode t)

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

\(fn &rest _)" t nil)

(autoload 'turn-off-solaire-mode "solaire-mode" "\
Disable `solaire-mode' in the current buffer.

\(fn &rest _)" t nil)

(autoload 'solaire-mode-reset "solaire-mode" "\
Reset `solaire-mode' in all buffers where it is enabled.

Use this in case solaire-mode has caused some sort of problem, e.g. after
changing themes.  are more prelevant in Emacs 25 and 26, but far less so in 27+;
particularly where the fringe is concerned.

\(fn &rest _)" t nil)

(autoload 'solaire-mode-reset-buffer "solaire-mode" "\
Reset `solaire-mode' incurrent buffer.

See `solaire-mode-reset' for details." nil nil)

(defun solaire-mode--prepare-for-theme-a (theme &rest _) "\
Prepare solaire-mode for THEME.
Meant to be used as a `load-theme' advice." (when (and (get theme 'theme-feature) (memq theme custom-enabled-themes)) (setq solaire-mode--supported-p (ignore-errors (let ((default1 (face-background 'default nil t)) (default2 (face-background 'solaire-default-face nil t))) (and default1 default2 (not (equal default1 default2))))) solaire-mode--swapped-p nil solaire-mode--theme theme) (when (bound-and-true-p solaire-global-mode) (if solaire-mode--supported-p (solaire-mode-swap-faces-maybe) (solaire-global-mode -1)))))

(advice-add #'load-theme :after #'solaire-mode--prepare-for-theme-a)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "solaire-mode" '("solaire-mode-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; solaire-mode-autoloads.el ends here
