;;; wgrep-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wgrep" "wgrep.el" (0 0 0 0))
;;; Generated autoloads from wgrep.el

(autoload 'wgrep-setup "wgrep" "\
Setup wgrep preparation." nil nil)

(add-hook 'grep-setup-hook 'wgrep-setup)

(register-definition-prefixes "wgrep" '("wgrep-"))

;;;***

;;;### (autoloads nil "wgrep-ack" "wgrep-ack.el" (0 0 0 0))
;;; Generated autoloads from wgrep-ack.el

(autoload 'wgrep-ack-and-a-half-setup "wgrep-ack" nil nil nil)

(autoload 'wgrep-ack-setup "wgrep-ack" nil nil nil)

(add-hook 'ack-and-a-half-mode-hook 'wgrep-ack-and-a-half-setup)

(add-hook 'ack-mode-hook 'wgrep-ack-setup)

(register-definition-prefixes "wgrep-ack" '("wgrep-ack-"))

;;;***

;;;### (autoloads nil "wgrep-ag" "wgrep-ag.el" (0 0 0 0))
;;; Generated autoloads from wgrep-ag.el

(autoload 'wgrep-ag-setup "wgrep-ag" nil nil nil)

(add-hook 'ag-mode-hook 'wgrep-ag-setup)

(register-definition-prefixes "wgrep-ag" '("wgrep-ag-"))

;;;***

;;;### (autoloads nil "wgrep-helm" "wgrep-helm.el" (0 0 0 0))
;;; Generated autoloads from wgrep-helm.el

(autoload 'wgrep-helm-setup "wgrep-helm" nil nil nil)

(add-hook 'helm-grep-mode-hook 'wgrep-helm-setup)

(add-hook 'helm-occur-mode-hook 'wgrep-helm-setup)

(register-definition-prefixes "wgrep-helm" '("wgrep-helm-"))

;;;***

;;;### (autoloads nil "wgrep-pt" "wgrep-pt.el" (0 0 0 0))
;;; Generated autoloads from wgrep-pt.el

(autoload 'wgrep-pt-setup "wgrep-pt" nil nil nil)

(add-hook 'pt-search-mode-hook 'wgrep-pt-setup)

(register-definition-prefixes "wgrep-pt" '("wgrep-pt-unload-function"))

;;;***

;;;### (autoloads nil "wgrep-subtest" "wgrep-subtest.el" (0 0 0 0))
;;; Generated autoloads from wgrep-subtest.el

(register-definition-prefixes "wgrep-subtest" '("wgrep-test--ag"))

;;;***

;;;### (autoloads nil "wgrep-test" "wgrep-test.el" (0 0 0 0))
;;; Generated autoloads from wgrep-test.el

(register-definition-prefixes "wgrep-test" '("wgrep-test"))

;;;***

;;;### (autoloads nil nil ("wgrep-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wgrep-autoloads.el ends here
