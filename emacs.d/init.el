;;; -*- lexical-binding: t; -*-

;;; init.el --- Summary
;;; Commentary:
;;; Code:

;; Keep track of loading time
(defconst emacs-start-time (current-time))

(setq amd/settings-file (expand-file-name "~/.emacs.d/README.el")
      amd/local-settings-file (expand-file-name "~/.emacs-local.el")
      amd/settings-org-file (expand-file-name "~/.emacs.d/README.org"))
(setq hostname (string-trim (shell-command-to-string "hostname")))
(setq amd/uname (shell-command-to-string "uname -a"))
(setq amd/using-android (string-match "Android" amd/uname))
(setq amd/using-pocketchip (string-match "chip" amd/uname))
(setq amd/using-pc (and (not amd/using-pocketchip)
                        (not amd/using-android)))

;; Don't garbage collect durring init
(setq gc-cons-threshold (if amd/using-pc most-positive-fixnum 256000000))

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Disable garbage collection when the minibuffer is active
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 16000000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; Initialize all ELPA packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/" ) t)

;; (package-initialize)

(if (>= emacs-major-version 27)
    (setq package-quickstart t))

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded packages, elapsed time: %.3fs" elapsed))

;; Load use-package, used for loading packages
(require 'use-package)

;; Only tangle README.org if README.el isn't available
;; Manually tangle with (amd/tangle-init) when needed.
(if (file-exists-p amd/settings-file)
    (load amd/settings-file)
  (progn
    (org-babel-load-file amd/settings-org-file)))


;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded settings, elapsed time: %.3fs" elapsed))

;; Loading done, restore gc-cons-threshold
(setq gc-cons-threshold 16000000)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024))

;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t)
  (message "Native complation is *not* available"))

(unless (functionp 'json-serialize)
  (message "Native JSON is *not* available"))

;; do not steal focus while doing async compilations
(setq warning-suppress-types '((comp)))

(when (file-exists-p amd/local-settings-file)
  (load amd/local-settings-file))

;; lastly, start a server
(unless (and (fboundp 'server-running-p) (server-running-p))
  (server-start))

