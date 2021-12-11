;;; -*- lexical-binding: t; -*-

(require 's)

;; (defconst pico8-colors
;;   '((?^ . "white")
;;     (?a . "light")
;;     (?b . "dark")
;;     (?. . "black"))
;;   "Alist of color associations for tiles.")

;; (defconst pico8-palette
;;   '(("white" . "#ffffff")
;;     ("light" . "#64b0ff")
;;     ("dark"  . "#4240ff")
;;     ("black" . "#000000"))
;;   "Default color palette for the XPM image.")

(defface pico8-face-black       '((t :foreground "black"   :background "#000000")) "black"       :group 'pico8-mode )
(defface pico8-face-dark_blue   '((t :foreground "black"   :background "#1D2B53")) "dark_blue"   :group 'pico8-mode )
(defface pico8-face-dark_purple '((t :foreground "black"   :background "#7E2553")) "dark_purple" :group 'pico8-mode )
(defface pico8-face-dark_green  '((t :foreground "black"   :background "#008751")) "dark_green"  :group 'pico8-mode )
(defface pico8-face-brown       '((t :foreground "black"   :background "#AB5236")) "brown"       :group 'pico8-mode )
(defface pico8-face-dark_gray   '((t :foreground "black"   :background "#5F574F")) "dark_gray"   :group 'pico8-mode )
(defface pico8-face-light_gray  '((t :foreground "black"   :background "#C2C3C7")) "light_gray"  :group 'pico8-mode )
(defface pico8-face-white       '((t :foreground "#6b6b6b" :background "#FFF1E8")) "white"       :group 'pico8-mode )
(defface pico8-face-red         '((t :foreground "#8b0000" :background "#FF004D")) "red"         :group 'pico8-mode )
(defface pico8-face-orange      '((t :foreground "black"   :background "#FFA300")) "orange"      :group 'pico8-mode )
(defface pico8-face-yellow      '((t :foreground "black"   :background "#FFEC27")) "yellow"      :group 'pico8-mode )
(defface pico8-face-green       '((t :foreground "black"   :background "#00E436")) "green"       :group 'pico8-mode )
(defface pico8-face-blue        '((t :foreground "black"   :background "#29ADFF")) "blue"        :group 'pico8-mode )
(defface pico8-face-indigo      '((t :foreground "black"   :background "#83769C")) "indigo"      :group 'pico8-mode )
(defface pico8-face-pink        '((t :foreground "#b03060" :background "#FF77A8")) "pink"        :group 'pico8-mode )
(defface pico8-face-peach       '((t :foreground "#b8860b" :background "#FFCCAA")) "peach"       :group 'pico8-mode )

(defface pico8-face-underline-black       '((t :underline "#000000")) "black"       :group 'pico8-mode )
(defface pico8-face-underline-dark_blue   '((t :underline "#1D2B53")) "dark_blue"   :group 'pico8-mode )
(defface pico8-face-underline-dark_purple '((t :underline "#7E2553")) "dark_purple" :group 'pico8-mode )
(defface pico8-face-underline-dark_green  '((t :underline "#008751")) "dark_green"  :group 'pico8-mode )
(defface pico8-face-underline-brown       '((t :underline "#AB5236")) "brown"       :group 'pico8-mode )
(defface pico8-face-underline-dark_gray   '((t :underline "#5F574F")) "dark_gray"   :group 'pico8-mode )
(defface pico8-face-underline-light_gray  '((t :underline "#C2C3C7")) "light_gray"  :group 'pico8-mode )
(defface pico8-face-underline-white       '((t :underline "#FFF1E8")) "white"       :group 'pico8-mode )
(defface pico8-face-underline-red         '((t :underline "#FF004D")) "red"         :group 'pico8-mode )
(defface pico8-face-underline-orange      '((t :underline "#FFA300")) "orange"      :group 'pico8-mode )
(defface pico8-face-underline-yellow      '((t :underline "#FFEC27")) "yellow"      :group 'pico8-mode )
(defface pico8-face-underline-green       '((t :underline "#00E436")) "green"       :group 'pico8-mode )
(defface pico8-face-underline-blue        '((t :underline "#29ADFF")) "blue"        :group 'pico8-mode )
(defface pico8-face-underline-indigo      '((t :underline "#83769C")) "indigo"      :group 'pico8-mode )
(defface pico8-face-underline-pink        '((t :underline "#FF77A8")) "pink"        :group 'pico8-mode )
(defface pico8-face-underline-peach       '((t :underline "#FFCCAA")) "peach"       :group 'pico8-mode )

(setq pico8-xpm-header
"/* XPM */
static char *graphic[] = {
\"%s %s 16 1\",
\"0	c #000000\", /* black       */
\"1	c #1D2B53\", /* dark_blue   */
\"2	c #7E2553\", /* dark_purple */
\"3	c #008751\", /* dark_green  */
\"4	c #AB5236\", /* brown       */
\"5	c #5F574F\", /* dark_gray   */
\"6	c #C2C3C7\", /* light_gray  */
\"7	c #FFF1E8\", /* white       */
\"8	c #FF004D\", /* red         */
\"9	c #FFA300\", /* orange      */
\"a	c #FFEC27\", /* yellow      */
\"b	c #00E436\", /* green       */
\"c	c #29ADFF\", /* blue        */
\"d	c #83769C\", /* indigo      */
\"e	c #FF77A8\", /* pink        */
\"f	c #FFCCAA\", /* peach       */
"
)

(setq pico8-mode-font-lock-keywords
      `(
        (,(regexp-opt '("sget" "sset" "fget" "fset" "clip" "print" "cursor" "color" "cls" "camera" "circ" "circfill" "line" "rect" "rectfill" "pal" "palt" "spr" "sspr" "btn" "btnp" "pset" "pget" "sfx" "music" "mset" "mget" "peek" "poke" "memcpy" "reload" "cstore" "memset" "min" "max" "mid" "flr" "cos" "sin" "atan2" "sqrt" "abs" "rnd" "srand" "band" "bor" "bxor" "bnot" "shr" "shl" "menuitem" "cartdata" "dget" "dset") 'symbols)
         . font-lock-builtin-face)
        ("[^\\.]\\(\\<0\\>\\)"  1 'pico8-face-underline-black      )
        ("[^\\.]\\(\\<1\\>\\)"  1 'pico8-face-underline-dark_blue  )
        ("[^\\.]\\(\\<2\\>\\)"  1 'pico8-face-underline-dark_purple)
        ("[^\\.]\\(\\<3\\>\\)"  1 'pico8-face-underline-dark_green )
        ("[^\\.]\\(\\<4\\>\\)"  1 'pico8-face-underline-brown      )
        ("[^\\.]\\(\\<5\\>\\)"  1 'pico8-face-underline-dark_gray  )
        ("[^\\.]\\(\\<6\\>\\)"  1 'pico8-face-underline-light_gray )
        ("[^\\.]\\(\\<7\\>\\)"  1 'pico8-face-underline-white      )
        ("[^\\.]\\(\\<8\\>\\)"  1 'pico8-face-underline-red        )
        ("[^\\.]\\(\\<9\\>\\)"  1 'pico8-face-underline-orange     )
        ("[^\\.]\\(\\<10\\>\\)" 1 'pico8-face-underline-yellow     )
        ("[^\\.]\\(\\<11\\>\\)" 1 'pico8-face-underline-green      )
        ("[^\\.]\\(\\<12\\>\\)" 1 'pico8-face-underline-blue       )
        ("[^\\.]\\(\\<13\\>\\)" 1 'pico8-face-underline-indigo     )
        ("[^\\.]\\(\\<14\\>\\)" 1 'pico8-face-underline-pink       )
        ("[^\\.]\\(\\<15\\>\\)" 1 'pico8-face-underline-peach      )
        )
      )

(setq pico8-sprite-mode-font-lock-keywords
      `(
        ("0" . 'pico8-face-black      )
        ("1" . 'pico8-face-dark_blue  )
        ("2" . 'pico8-face-dark_purple)
        ("3" . 'pico8-face-dark_green )
        ("4" . 'pico8-face-brown      )
        ("5" . 'pico8-face-dark_gray  )
        ("6" . 'pico8-face-light_gray )
        ("7" . 'pico8-face-white      )
        ("8" . 'pico8-face-red        )
        ("9" . 'pico8-face-orange     )
        ("a" . 'pico8-face-yellow     )
        ("b" . 'pico8-face-green      )
        ("c" . 'pico8-face-blue       )
        ("d" . 'pico8-face-indigo     )
        ("e" . 'pico8-face-pink       )
        ("f" . 'pico8-face-peach      )
        )
      )

;; (defvar hexcolour-keywords
;;   '(("#[abcdef[:digit:]]\\{6\\}"
;;      (0 (put-text-property (match-beginning 0)
;;                            (match-end 0)
;;                            'face (list :background
;;                                        (match-string-no-properties 0)))))))

(defun pico8-end-of-sprite-sheet ()
  (search-forward "\n__"))

(define-derived-mode pico8-sprite-mode fundamental-mode "Pico-8 Sprite"
  "major mode for editing pico8 code."
  (font-lock-add-keywords nil pico8-sprite-mode-font-lock-keywords)
)

(define-derived-mode pico8-mode lua-mode "Pico-8"
  "major mode for editing pico8 code."
  (font-lock-add-keywords nil pico8-mode-font-lock-keywords)
  ;; (font-lock-add-keywords nil '(("\\<\\(FIXME\\):" 1 '(:foreground "blue") t)))

  ;; (defun my-end-of-paragraph-position (&rest foo)
  ;;   "Return position of next empty line."
  ;;   (save-excursion
  ;;     (while (not (or (eobp)             ; Stop at end of buffer.
  ;;                     (and (bolp)        ; Or at an empty line.
  ;;                          (eolp))))
  ;;       (forward-line))
  ;;     (point)))

  ;; (setq font-lock-multiline t)

  ;; (font-lock-add-keywords nil
  ;;                         '(
  ;;                           ("^__gfx__"
  ;;                            (0 font-lock-keyword-face)  ;; Face for FOO
  ;;                            ("c"
  ;;                             pico8-end-of-sprite-sheet
  ;;                             nil
  ;;                             (0 pico8-face-blue)))
  ;;                           )
  ;;                         )

)

;; (defun run-pico8 ()
;;   "run a pico-8 cartridge then revert buffer"
;;   (interactive)
;;   (let ((current-file-path (file-name-base (buffer-file-name (current-buffer))))
;;         (pico8-command     (cond ((eq system-type 'cygwin)
;;                                   "/home/anthony/pico-8_win32/pico8.exe -windowed 1 -home 'C:\cygwin64\home\anthony\heliopause-pico-8' -run "
;;                                   ;; "/home/anthony/pico-8_win32/pico8.exe -windowed 1 -home C:/cygwin64/home/anthony/heliopause-pico-8 "
;;                                   )
;;                                  ((eq system-type 'windows-nt)
;;                                   "c:/Users/anthony/pico-8_win32/pico8.exe -windowed 1 -home C:\\Users\\anthony\\heliopause-pico-8 -run "
;;                                   )
;;                                  (amd/using-pocketchip
;;                                   "/usr/lib/pico8/pico8 -run ")
;;                                  (t
;;                                   "/home/anthony/apps/pico-8/pico8 -run "))))
;;     (save-buffer)
;;     (shell-command (concat pico8-command current-file-path))
;;     ;; (shell-command pico8-command)
;;     (revert-buffer nil t)))

(defun pico8-token-count ()
  "Return token counts"
  (interactive)

  (save-mark-and-excursion
    (goto-char (point-min))
    (search-forward "\n__lua__")
    (beginning-of-line)
    (forward-line 1)
    (setq pico8-code-start (point))
    (search-forward "\n__gfx__")
    (forward-line -1)
    (end-of-line)
    (setq pico8-code-end (point))
    (let ((old-buffer (current-buffer)))
      (with-temp-buffer
        (insert-buffer-substring-no-properties old-buffer pico8-code-start pico8-code-end)
        (goto-char (point-min))
        (replace-regexp "--\\[\\[\\(.*\n\\)*.*\\]\\]" "")
        (goto-char (point-min))
        (replace-regexp "--.*$" "")
        (goto-char (point-min))
        (replace-regexp "\"\\(\\\\\"\\|.\\)*?\"" "\"\"")
        (goto-char (point-min))
        (replace-regexp "'\\(\\\\'\\|.\\)*?'" "''")
        (lua-mode)
        (goto-char (point-min))
        (let*
            ((words           (how-many "\\_<[A-Za-z0-9_]+\\_>" ))
             (operators       (how-many "\\([-*+\\/%><~!]=?\\|[^<>~!]=[^=]\\)" ))
             (quotes          (how-many "[\"'][\"']"))
             (delimiters      (how-many "[({\\[]" ))
             (locals          (how-many "\\_<local\\_>" ))
             (ends            (how-many "\\_<end\\_>" ))
             (decimal-numbers (how-many "\\<[0-9]+\\.[0-9]+\\>" ))
             (others          (how-many "\\(::\\|#\\|\\.\\.\\.\\|[^\\.]\\.\\.[^\\.]\\)" ))
             )
          (message "%d" (- (+ words delimiters operators quotes others) locals ends decimal-numbers))
          )
        )
      )
    )
  )

(defun pico8-generate-xpm-body ()
  "Returns a string resembling a valid XPM body."
  (interactive)
  (save-excursion
   (goto-char (point-min))
   (search-forward "\n__gfx__")
   (forward-line 1)
   (beginning-of-line)
   (setq img-start (point))
   (search-forward "\n__")
   (forward-line -1)
   (end-of-line)
   (setq img-end (point))
   (let ((old-buffer (current-buffer)))
     (with-temp-buffer
     ;; (with-current-buffer "*pico8test*"
       (insert-buffer-substring-no-properties old-buffer img-start img-end)
       (goto-char (point-min))
       (end-of-line)
       (setq line-length (- (point) 1))
       (goto-char (point-min))
       (setq more-lines t)
       (while more-lines
         (beginning-of-line)
         (dotimes (i line-length)
           (forward-char 1)
           (insert (char-before)))

         (beginning-of-line)
         (setq line-start (point))
         (insert "\"")
         (end-of-line)
         (insert "\",")
         (end-of-line)
         (setq line-end (point))
         (insert (buffer-substring line-start line-end))
         (setq more-lines (= 0 (forward-line 1)))
         )
       (insert "}")
       (goto-char (point-min))
       (insert (format pico8-xpm-header 256 256))
       (setq img (create-image (buffer-string) 'xpm t))
       )
     )
   (goto-char (point-min))
   (search-forward "\n__gfx__")
   (forward-line -1)
   (insert-image img)
   )
  )


;; (with-current-buffer "heliopause.p8"
;;   (how-many (rx (sequence symbol-start "function" symbol-end))))

;; (with-current-buffer "heliopause.p8"
;;   (how-many (rx (char "[{("))))

(provide 'pico8)
;;; pico8.el ends here
