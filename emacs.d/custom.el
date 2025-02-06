(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(doom-modeline-bar-width 8)
 '(doom-modeline-height 1)
 '(doom-modeline-icon nil)
 '(elfeed-feeds
   '("https://hackaday.com/blog/feed/" "http://planet.emacslife.com/atom.xml"))
 '(inhibit-startup-screen nil)
 '(mac-mouse-wheel-smooth-scroll nil)
 '(magit-log-arguments '("--graph" "--color" "--decorate"))
 '(magit-status-buffer-switch-function 'switch-to-buffer)
 '(package-selected-packages
   '(vterm elfeed flycheck-rust page-break-lines catppuccin-theme bazel treesit-auto org-superstar org embark embark-consult consult marginalia vertico glsl-mode transient-posframe typescript-mode company-lsp jsonrpc counsel-gtags ggtags ibuffer-projectile highlight-indent-guides major-mode-hydra airline-themes hide-mode-line markdown-mode+ markdown-mode yasnippet gif-screencast gn-mode ox-gfm fennel-mode blimp ob-async dts-mode forth-mode jinja2-mode request fill-column-indicator sqlup-mode scad-mode scad-preview steam ssh-config-mode ssh-agency general ht paradox sql-indent pylint moonscript xterm-color vimrc-mode company-shell vlf exec-path-from-shell edit-server json-mode yari yaml-mode wgrep-pt subatomic256-theme scss-mode rainbow-delimiters pt pandoc-mode js2-mode htmlize haml-mode go-mode feature-mode esup emmet-mode edbi company-inf-ruby coffee-mode ag))
 '(paradox-github-token t)
 '(projectile-mode-line '(:eval (format " p[%s]" (projectile-project-name))))
 '(python-indent-guess-indent-offset t)
 '(safe-local-variable-values
   '((org-html-table-default-attributes :class "mdl-data-table mdl-js-data-table mdl-data-table--selectable mdl-shadow--2dp" :border "1" :cellspacing "0" :cellpadding "0" :rules "all" :frame "border")
     (org-html-toplevel-hlevel . 1)
     (org-html-table-default-attributes :border "1" :cellspacing "0" :cellpadding "0" :rules "all" :frame "border")
     (org-html-table-data-tags "<td class=\"mdl-data-table__cell--non-numeric\" %s>" . "</td>")
     (org-html-table-header-tags "<th class=\"mdl-data-table__cell--non-numeric\" scope=\"%s\"%s>" . "</th>")
     (org-html-todo-kwd-class-prefix . "mdl-chip mdl-chip--contact ")))
 '(scss-compile-at-save nil)
 '(yari-ri-program-name "~/.rbenv/shims/ri")
 '(yari-ruby-program-name "~/.rbenv/shims/ruby"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:family "Roboto")))))
