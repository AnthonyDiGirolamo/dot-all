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
   '("https://hackaday.com/blog/feed/"
     "http://planet.emacslife.com/atom.xml"))
 '(mac-mouse-wheel-smooth-scroll nil)
 '(magit-log-arguments '("--graph" "--color" "--decorate"))
 '(magit-status-buffer-switch-function 'switch-to-buffer)
 '(package-selected-packages
   '(ag airline-themes bazel blimp coffee-mode company-inf-ruby
        company-lsp company-shell counsel-gtags dts-mode edbi
        edit-server elfeed emmet-mode esup evil exec-path-from-shell
        feature-mode fennel-mode fill-column-indicator flycheck-rust
        forth-mode general ggtags gif-screencast glsl-mode gn-mode
        go-mode haml-mode hide-mode-line ht htmlize ibuffer-projectile
        jinja2-mode js2-mode json-mode jsonrpc llama major-mode-hydra
        markdown-mode markdown-mode+ moonscript ob-async org-superstar
        ox-gfm page-break-lines pandoc-mode paradox pt pylint request
        scad-mode scad-preview scss-mode sql-indent sqlup-mode
        ssh-agency ssh-config-mode steam subatomic256-theme transient
        transient-posframe treesit-auto typescript-mode vimrc-mode vlf
        vterm wgrep-pt which-key with-editor xterm-color yaml-mode
        yari yasnippet))
 '(paradox-github-token t)
 '(projectile-mode-line '(:eval (format " p[%s]" (projectile-project-name))))
 '(python-indent-guess-indent-offset t)
 '(safe-local-variable-values
   '((eval setq lsp-clients-clangd-executable
           (expand-file-name
            (concat (projectile-project-root)
                    "/prebuilt/third_party/clang/linux-x64/bin/clangd")))
     (eval add-to-list 'exec-path
           (expand-file-name
            (concat (projectile-project-root)
                    "/prebuilt/third_party/clang/linux-x64/bin")))
     (eval add-to-list 'exec-path
           (expand-file-name
            (concat (projectile-project-root)
                    "/environment/cipd/packages/pigweed/bin")))
     (lsp-clients-clangd-args "--background-index=true")
     (org-html-table-default-attributes :class
                                        "mdl-data-table mdl-js-data-table mdl-data-table--selectable mdl-shadow--2dp"
                                        :border "1" :cellspacing "0"
                                        :cellpadding "0" :rules "all"
                                        :frame "border")
     (org-html-toplevel-hlevel . 1)
     (org-html-table-default-attributes :border "1" :cellspacing "0"
                                        :cellpadding "0" :rules "all"
                                        :frame "border")
     (org-html-table-data-tags
      "<td class=\"mdl-data-table__cell--non-numeric\" %s>" . "</td>")
     (org-html-table-header-tags
      "<th class=\"mdl-data-table__cell--non-numeric\" scope=\"%s\"%s>"
      . "</th>")
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
