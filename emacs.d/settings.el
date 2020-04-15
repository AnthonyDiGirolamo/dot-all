;; amd/variables

(setq amd/leader-key ",")
(setq amd/leader-key-insert "C-,")
(when amd/using-pocketchip
  (setq amd/leader-key "SPC"))

;; path

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; general settings

(require 'mouse)
(xterm-mouse-mode t)

(setq echo-keystrokes 0.2)

(setq ring-bell-function (lambda ()))
(setq recenter-redisplay nil) ;; don't redraw the whole display when recentering

(setq-default fill-column 80)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill) ;; get auto line breaks at fill-column - auto-fill-mode
(set-display-table-slot standard-display-table 'wrap ?\ ) ;; Hide the \ at the end of each wrapped line. Don't reall need it with relative-line-numbers
;; toggle-truncate-lines will toggle line wrapping
;; auto-fill-mode will insert line breaks automatically

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode -1)

;; Save Tempfiles in a temp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Stop making backup files
(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p) ;; no more typing out y.e.s.

;; (set-default 'show-trailing-whitespace t)
(setq delete-trailing-lines nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Erase trailing whitespace before save

;; Indentation
(setq-default c-basic-indent 4)
(setq-default tab-width 4)          ;; set tw=4
(setq-default indent-tabs-mode nil) ;; set expandtab

;; Scroll just one line when hitting bottom of window
(setq scroll-step 1)
(setq scroll-conservatively 101)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil            ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't)                 ;; scroll window under mouse

;; (setq mouse-wheel-flip-direction nil) ;; Swap left/right scroll direction

;; Swap up/down scroll direction
(setq mouse-wheel-down-event 'mouse-5)
(setq mouse-wheel-up-event 'mouse-4)

;; cursor movement lag reduction?
(setq auto-window-vscroll nil)

;; Mac OSX Emacs Settings
(setq ns-alternate-modifier 'meta
      ns-command-modifier 'meta)

;; UTF8 Setup
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp buffer-file-coding-system)
    (setq buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
