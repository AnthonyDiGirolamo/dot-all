(setq initial-buffer-choice nil ;; Open *scratch* buffer by default
      ;; initial-major-mode 'fundamental-mode
      inhibit-startup-message t
      inhibit-startup-screen t)

;; Start Maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Keep track of loading time
(defconst emacs-start-time (current-time))

(setq amd/settings-file (expand-file-name "~/.emacs.d/README.el")
      amd/settings-org-file (expand-file-name "~/.emacs.d/README.org"))
(setq amd/uname (shell-command-to-string "uname -a"))
(setq amd/using-android (string-match "Android" amd/uname))
(setq amd/using-pocketchip (string-match "chip" amd/uname))
(setq amd/using-chromebook (string-match "galliumos" amd/uname))
(setq amd/using-pc (and (not amd/using-pocketchip)
                        (not amd/using-android)))

;; Don't garbage collect durring init
(setq gc-cons-threshold (if amd/using-pc most-positive-fixnum 256000000))

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

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
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/" ) t)
(setq package-enable-at-startup nil
      package--init-file-ensured t)

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

;; (defun my-tangle-config-org (src-file output-file)
;;   "This function will write all source blocks from =config.org= into
;; =config.el= that are ...
;; - not marked as =tangle: no=
;; - doesn't have the TODO state =CANCELLED=
;; - have a source-code of =emacs-lisp="
;;   (require 'org)
;;   (let* ((body-list ())
;;          (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
;;                                                                 (list (cons :tangle output-file)))))
;;     (message "Writing %s ..." output-file)
;;     (save-restriction
;;      (save-excursion
;;       (org-babel-map-src-blocks src-file
;;                                 (let* ((info (org-babel-get-src-block-info 'light))
;;                                        (tfile (cdr (assq :tangle (nth 2 info))))
;;                                        (match))
;;                                   (save-excursion
;;                                    (catch 'exit
;;                                      (org-back-to-heading t)
;;                                      (when (looking-at org-outline-regexp)
;;                                        (goto-char (1- (match-end 0))))
;;                                      (when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
;;                                        (setq match (match-string 1)))))
;;                                   (unless (or (string= "no" tfile)
;;                                               (string= "CANCELED" match)
;;                                               (not (string= "emacs-lisp" lang)))
;;                                     (add-to-list 'body-list body)))))
;;      (with-temp-file output-file
;;        (insert (format ";; Don't edit this file, edit '%s' instead.\n\n" src-file))
;;        (insert (apply 'concat (reverse body-list))))
;;      (message "Wrote %s" output-file))))

;; (load amd/settings-file)
;; (message (concat "Done loading " amd/settings-org-file))

;; Load early settings.el
(load (expand-file-name "~/.emacs.d/settings.el"))

(if (and amd/using-pocketchip (file-exists-p amd/settings-file))
    (load amd/settings-file)
  (progn
    (org-babel-load-file amd/settings-org-file)
    ;; (my-tangle-config-org amd/settings-org-file amd/settings-file)
    ;; (load amd/settings-file)
    (message (concat "Done loading " amd/settings-org-file))))

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded settings, elapsed time: %.3fs" elapsed))

;; ;; restore gc-cons-threshold
;; (setq gc-cons-threshold 16000000)

;; Open org-default-notes-file
;; (when (file-exists-p org-default-notes-file)
;;   (find-file org-default-notes-file))
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
