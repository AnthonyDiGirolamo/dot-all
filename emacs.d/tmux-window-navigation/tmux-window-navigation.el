;;; tmux-window-navigation.el -*- lexical-binding: t; -*-

;; Author: Anthony DiGirolamo <anthony.digirolamo@gmail.com>
;; Version: 0.1
;; Keywords: tmux, window, plugin

;;; Commentary:
;;
;; Provides functions for navigating in and out of tmux panes when
;; running emacs within a tmux session.
;;
;;; Installation:
;;
;; (use-package tmux-window-navigation
;;   :config
;;   (global-tmux-window-navigation-mode 1))

;;; Code:

;;;###autoload
(define-minor-mode tmux-window-navigation-mode
  "Window swaping and navigation with tmux integration"
  :init-value nil
  :lighter ""
  :keymap (make-sparse-keymap))

;;;###autoload
(defun tmux-window-navigation-mode-install () (tmux-window-navigation-mode 1))

;;;###autoload
(define-globalized-minor-mode global-tmux-window-navigation-mode
  tmux-window-navigation-mode tmux-window-navigation-mode-install
  "Window swaping and navigation with tmux integration")

;; (defun tmux-window-navigation-setup-hook ()
;;   (tmux-window-navigation-mode 0))
;; (add-hook 'minibuffer-setup-hook 'tmux-window-navigation-setup-hook)

;; simple tiling
(defun tmux-window-navigation/swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when (and other-window (not (eq other-window (minibuffer-window))))
      (let* ((this-window  (selected-window))
             (this-buffer  (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start   (window-start this-window))
             (other-start  (window-start other-window)))
        (set-window-buffer this-window  other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start  this-window  other-start)
        (set-window-start  other-window this-start)))))

(defun tmux-window-navigation/tmux-navigate (direction)
  (unless (minibufferp)
    (let
        ((cmd (concat "windmove-" direction)))
      (condition-case nil
          (funcall (read cmd))
        (error
         (tmux-window-navigation/tmux-command direction))))))
(defun tmux-window-navigation/tmux-command (direction)
  (shell-command-to-string
   (concat "tmux select-pane -"
           (tmux-window-navigation/tmux-direction direction))))
(defun tmux-window-navigation/tmux-direction (direction)
  (upcase
   (substring direction 0 1)))

(defun tmux-window-navigation/move-down  () (interactive) (tmux-window-navigation/tmux-navigate "down"))
(defun tmux-window-navigation/move-up    () (interactive) (tmux-window-navigation/tmux-navigate "up"))
(defun tmux-window-navigation/move-left  () (interactive) (tmux-window-navigation/tmux-navigate "left"))
(defun tmux-window-navigation/move-right () (interactive) (tmux-window-navigation/tmux-navigate "right"))

(define-key tmux-window-navigation-mode-map (kbd "M-n") 'tmux-window-navigation/move-down)
(define-key tmux-window-navigation-mode-map (kbd "M-e") 'tmux-window-navigation/move-up)
(define-key tmux-window-navigation-mode-map (kbd "M-h") 'tmux-window-navigation/move-left)
(define-key tmux-window-navigation-mode-map (kbd "M-l") 'tmux-window-navigation/move-right)

(defun tmux-window-navigation/swap-down  () (interactive) (tmux-window-navigation/swap-with 'down)  (windmove-down))
(defun tmux-window-navigation/swap-up    () (interactive) (tmux-window-navigation/swap-with 'up)    (windmove-up))
(defun tmux-window-navigation/swap-left  () (interactive) (tmux-window-navigation/swap-with 'left)  (windmove-left))
(defun tmux-window-navigation/swap-right () (interactive) (tmux-window-navigation/swap-with 'right) (windmove-right))

(define-key tmux-window-navigation-mode-map (kbd "C-M-n") 'tmux-window-navigation/swap-down)
(define-key tmux-window-navigation-mode-map (kbd "C-M-e") 'tmux-window-navigation/swap-up)
(define-key tmux-window-navigation-mode-map (kbd "C-M-h") 'tmux-window-navigation/swap-left)
(define-key tmux-window-navigation-mode-map (kbd "C-M-l") 'tmux-window-navigation/swap-right)

(define-key tmux-window-navigation-mode-map (kbd "M-N") (lambda () (interactive) (enlarge-window 1)))
(define-key tmux-window-navigation-mode-map (kbd "M-E") (lambda () (interactive) (enlarge-window -1)))
(define-key tmux-window-navigation-mode-map (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
(define-key tmux-window-navigation-mode-map (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))

;; (define-key tmux-window-navigation-mode-map (kbd "M-n") 'windmove-down)
;; (define-key tmux-window-navigation-mode-map (kbd "M-e") 'windmove-up)
;; (define-key tmux-window-navigation-mode-map (kbd "M-h") 'windmove-left)
;; (define-key tmux-window-navigation-mode-map (kbd "M-l") 'windmove-right)

(defun amd/tmux-dispatch-below (dispatch-text)
  (interactive "sText: ")
  (require 's)
  (let* ((tmux-panes        (shell-command-to-string "tmux list-panes"))
         (tmux-pane-number  (s-chomp (shell-command-to-string "tmux list-panes|grep -v active|head -n 1|cut -d: -f1")))
         (text-to-send      (s-trim (s-chomp dispatch-text)))
         (tmux-send-command (concat "tmux send -t " tmux-pane-number " \'" text-to-send "\'")))
    ;; (shell-command-to-string tmux-send-command)))
    (shell-command-to-string (concat "tmux send -t " tmux-pane-number " 'C-c'"))
    (shell-command-to-string (concat tmux-send-command "\r"))))

(defun amd/tmux-dispatch-below-reigon (beg end)
  (interactive "r")
  (amd/tmux-dispatch-below (buffer-substring-no-properties beg end)))

(provide 'tmux-window-navigation)
;;; tmux-window-navigation.el ends here
