;;; ninja-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ninja-mode" "ninja-mode.el" (0 0 0 0))
;;; Generated autoloads from ninja-mode.el

(autoload 'ninja-mode "ninja-mode" "\


\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ninja$" . ninja-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ninja-mode" '("ninja-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ninja-mode-autoloads.el ends here
