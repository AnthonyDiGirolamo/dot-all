

;; Author: Anthony DiGirolamo <anthony.digirolamo@gmail.com>
;; URL: http://github.com/AnthonyDiGirolamo/tmux-window-navigation
;; Version: 0.1
;; Keywords: tmux, window, plugin

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides some extra operators for Emacs Evil, to evaluate codes,
;; search via google, translate text, folding region, etc.
;;
;; Installation:
;;
;; put tmux-window-navigation.el somewhere in your load-path and add these
;; lines to your .emacs:
;; (require 'tmux-window-navigation)
;; (global-tmux-window-navigation-mode 1)

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

(defun amd/tmux-dispatch-below (beg end)
  (interactive "r")
  (require 's)
  (let* ((tmux-panes        (shell-command-to-string "tmux list-panes"))
         (tmux-pane-number  (s-chomp (shell-command-to-string "tmux list-panes|grep -v active|head -n 1|cut -d: -f1")))
         (text-to-send      (s-trim (s-chomp (buffer-substring-no-properties beg end))))
         (tmux-send-command (concat "tmux send -t " tmux-pane-number " \'" text-to-send "\'")))
    ;; (shell-command-to-string tmux-send-command)))
    (shell-command-to-string (concat "tmux send -t " tmux-pane-number " 'C-c'"))
    (shell-command-to-string (concat tmux-send-command "\r"))))

(provide 'tmux-window-navigation)
;;; tmux-window-navigation.el ends here
