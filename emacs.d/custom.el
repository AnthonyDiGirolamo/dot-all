(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(elfeed-feeds
   (quote
    ("https://hackaday.com/blog/feed/" "http://planet.emacsen.org/atom.xml")))
 '(elscreen-display-tab nil)
 '(inhibit-startup-screen nil)
 '(mac-mouse-wheel-smooth-scroll nil)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate")))
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(package-selected-packages
   (quote
    (ibuffer-projectile highlight-indent-guides major-mode-hydra airline-themes hide-mode-line modus-operandi-theme modus-vivendi-theme markdown-mode+ markdown-mode yasnippet gif-screencast lsp-mode gn-mode ninja-mode gnuplot ox-gfm fennel-mode blimp use-package ob-async ivy-xref dts-mode forth-mode company-jedi jedi-core jinja2-mode mmm-jinja2 request fill-column-indicator sqlup-mode scad-mode scad-preview toc-org steam ssh-config-mode ssh-agency general ht paradox sql-indent pylint moonscript smex xterm-color fish-mode vimrc-mode company-shell vlf exec-path-from-shell edit-server evil-visualstar json-mode expand-region yari yaml-mode wgrep-pt wgrep-ag subatomic256-theme scss-mode robe rainbow-delimiters pt pandoc-mode js2-mode jedi htmlize haml-mode go-mode feature-mode evil-commentary esup emmet-mode edbi company-inf-ruby coffee-mode ag ace-link)))
 '(paradox-github-token t)
 '(projectile-mode-line (quote (:eval (format " p[%s]" (projectile-project-name)))))
 '(python-indent-guess-indent-offset t)
 '(safe-local-variable-values
   (quote
    ((org-html-toplevel-hlevel . 1)
     (org-html-table-default-attributes :border "1" :cellspacing "0" :cellpadding "0" :rules "all" :frame "border")
     (org-html-table-data-tags "<td class=\"mdl-data-table__cell--non-numeric\" %s>" . "</td>")
     (org-html-table-header-tags "<th class=\"mdl-data-table__cell--non-numeric\" scope=\"%s\"%s>" . "</th>")
     (org-html-todo-kwd-class-prefix . "mdl-chip mdl-chip--contact "))))
 '(scss-compile-at-save nil)
 '(yari-ri-program-name "~/.rbenv/shims/ri")
 '(yari-ruby-program-name "~/.rbenv/shims/ruby"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:family "Roboto")))))
