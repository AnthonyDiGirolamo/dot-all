;;; init.el --- Summary
;;; Commentary:
;;; Code:

;; Keep track of loading time
(defconst emacs-start-time (current-time))

(setq amd/settings-file (expand-file-name "~/.emacs.d/README.el")
      amd/settings-org-file (expand-file-name "~/.emacs.d/README.org"))
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
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; Don't package-initialize
;; (package-initialize)

(defvar cache-file "~/.emacs.d/cache/autoloads")

(defun initialize ()
  "Concatenate all package autoloads into cache-file then load."
  (unless (load cache-file t t)
    (setq package-activated-list nil)
    (package-initialize)
    (with-temp-buffer
      ;; (cl-pushnew doom-core-dir load-path :test #'string=)
      (dolist (desc (delq nil (mapcar #'cdr package-alist)))
        (let ((load-file-name (concat (package--autoloads-file-name (car desc)) ".el")))
          ;; (message "initialize: %s" load-file-name)
          (when (file-readable-p load-file-name)
            (condition-case _
                ;; (while t (insert (read (current-buffer))))
                (insert-file-contents load-file-name)
              (end-of-file)))))
      (prin1 `(setq load-path ',load-path
                    auto-mode-alist ',auto-mode-alist
                    Info-directory-list ',Info-directory-list)
             (current-buffer))
      (write-file (concat cache-file ".el"))
      (byte-compile-file (concat cache-file ".el")))))

(initialize)

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded packages, elapsed time: %.3fs" elapsed))

;; Load use-package, used for loading packages
(require 'use-package)

;; conditionally tangle README.org
(if (and amd/using-pocketchip (file-exists-p amd/settings-file))
    (load amd/settings-file)
  (progn
    (org-babel-load-file amd/settings-org-file)
    (message (concat "Done loading " amd/settings-org-file))))

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded settings, elapsed time: %.3fs" elapsed))

;; Loading done, restore gc-cons-threshold
(setq gc-cons-threshold 16000000)

