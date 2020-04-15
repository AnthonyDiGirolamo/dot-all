;;; forth-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "forth-block-mode" "forth-block-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from forth-block-mode.el

(autoload 'forth-block-mode "forth-block-mode" "\
Minor mode for Forth code in blocks.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forth-block-mode" '("forth-")))

;;;***

;;;### (autoloads nil "forth-interaction-mode" "forth-interaction-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from forth-interaction-mode.el

(autoload 'forth-kill "forth-interaction-mode" "\


\(fn &optional BUFFER)" t nil)

(autoload 'run-forth "forth-interaction-mode" "\
Start an interactive forth session.

\(fn)" t nil)

(autoload 'forth-restart "forth-interaction-mode" "\


\(fn)" t nil)

(autoload 'forth-interaction-send "forth-interaction-mode" "\


\(fn &rest STRINGS)" nil nil)

(autoload 'forth-words "forth-interaction-mode" "\


\(fn)" nil nil)

(autoload 'forth-eval "forth-interaction-mode" "\


\(fn STRING)" t nil)

(autoload 'forth-eval-region "forth-interaction-mode" "\


\(fn START END)" t nil)

(autoload 'forth-eval-defun "forth-interaction-mode" "\


\(fn)" t nil)

(autoload 'forth-load-file "forth-interaction-mode" "\


\(fn FILE)" t nil)

(autoload 'forth-see "forth-interaction-mode" "\


\(fn WORD)" t nil)

(autoload 'forth-switch-to-output-buffer "forth-interaction-mode" "\


\(fn)" t nil)

(autoload 'forth-switch-to-source-buffer "forth-interaction-mode" "\


\(fn)" t nil)

(autoload 'forth-eval-last-expression "forth-interaction-mode" "\


\(fn)" t nil)

(autoload 'forth-eval-last-expression-display-output "forth-interaction-mode" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forth-interaction-mode" '("forth-")))

;;;***

;;;### (autoloads nil "forth-mode" "forth-mode.el" (0 0 0 0))
;;; Generated autoloads from forth-mode.el

(autoload 'forth-mode "forth-mode" "\
Major mode for editing Forth files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(f\\|fs\\|fth\\|4th\\)\\'" . forth-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forth-mode" '("forth-")))

;;;***

;;;### (autoloads nil "forth-parse" "forth-parse.el" (0 0 0 0))
;;; Generated autoloads from forth-parse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forth-parse" '("forth-")))

;;;***

;;;### (autoloads nil "forth-smie" "forth-smie.el" (0 0 0 0))
;;; Generated autoloads from forth-smie.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forth-smie" '("forth-smie-")))

;;;***

;;;### (autoloads nil "forth-spec" "forth-spec.el" (0 0 0 0))
;;; Generated autoloads from forth-spec.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forth-spec" '("forth-spec-")))

;;;***

;;;### (autoloads nil "forth-syntax" "forth-syntax.el" (0 0 0 0))
;;; Generated autoloads from forth-syntax.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "forth-syntax" '("forth-syntax-")))

;;;***

;;;### (autoloads nil nil ("forth-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; forth-mode-autoloads.el ends here
