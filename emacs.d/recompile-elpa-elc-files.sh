#!/bin/sh
rm -f ~/.emacs.d/cache/autoload*
rm -f ~/.emacs.d/package-quickstart.el*
find ~/.emacs.d/elpa/ -iname "*.elc" -delete
find ~/.emacs.d/elpa/ -maxdepth 1 -type d -empty -delete
emacs -Q --batch --eval '(byte-recompile-directory "~/.emacs.d/elpa/" 0 t)'
time emacs -nw --eval '(progn (package-quickstart-refresh) (save-buffers-kill-terminal))'
time emacs -nw --eval '(save-buffers-kill-terminal)'
