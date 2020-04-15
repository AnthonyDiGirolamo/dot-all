;;; fennel-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fennel-mode" "fennel-mode.el" (0 0 0 0))
;;; Generated autoloads from fennel-mode.el

(autoload 'fennel-mode "fennel-mode" "\
Major mode for editing Fennel code.

\\{fennel-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fennel-mode" '("fennel-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fennel-mode-autoloads.el ends here
