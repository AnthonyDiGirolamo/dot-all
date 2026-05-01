;;; tokyo-night.el --- Shared infrastructure for Tokyo Night themes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/tokyo-night-emacs
;; Package-Version: 20260421.1234
;; Package-Revision: 8d30673c060a
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces themes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shared color palettes and face-setting infrastructure for the Tokyo Night
;; family of themes.  This file is not a theme itself -- it is required by
;; the individual theme files (tokyo-night-theme.el, etc.).
;;
;; Based on folke's Tokyo Night for Neovim:
;; https://github.com/folke/tokyonight.nvim

;;; Code:

(require 'cl-lib)

(defgroup tokyo-night nil
  "Tokyo Night theme family."
  :group 'faces
  :prefix "tokyo-"
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/tokyo-night-emacs")
  :tag "Tokyo Night")

(defcustom tokyo-night-scale-headings t
  "Whether to scale headings in org, outline, markdown, shr, and info.
Set to nil for uniform heading sizes.  Takes effect on theme load."
  :type 'boolean
  :group 'tokyo-night)

(defcustom tokyo-night-override-colors-alist '()
  "Alist of color overrides applied to all variants.
Each entry should be a cons cell (NAME . VALUE) where NAME is a
color name from any variant's palette and VALUE is the
replacement hex color string."
  :type '(alist :key-type string :value-type string)
  :group 'tokyo-night)

;;; Color Palettes

(defconst tokyo-night-colors-alist
  '(;; Background shades
    ("tokyo-bg-darkest"    . "#0C0E14")
    ("tokyo-bg-dark"       . "#16161e")
    ("tokyo-bg"            . "#1a1b26")
    ("tokyo-bg-highlight"  . "#292e42")
    ("tokyo-bg-line"       . "#1e202e")

    ;; Foreground shades
    ("tokyo-fg"            . "#c0caf5")
    ("tokyo-fg-dark"       . "#a9b1d6")
    ("tokyo-fg-muted"      . "#9aa5ce")
    ("tokyo-fg-gutter"     . "#3b4261")

    ;; UI elements
    ("tokyo-line-nr"       . "#363b54")
    ("tokyo-line-nr-cur"   . "#787c99")
    ("tokyo-indent"        . "#232433")
    ("tokyo-selection"     . "#515c7e")
    ("tokyo-dark3"         . "#545c7e")
    ("tokyo-dark5"         . "#737aa2")
    ("tokyo-terminal-blk"  . "#414868")
    ("tokyo-bracket"       . "#42465d")
    ("tokyo-whitespace"    . "#363b54")

    ;; Core accent colors
    ("tokyo-red"           . "#f7768e")
    ("tokyo-red-dark"      . "#db4b4b")
    ("tokyo-orange"        . "#ff9e64")
    ("tokyo-yellow"        . "#e0af68")
    ("tokyo-green"         . "#9ece6a")
    ("tokyo-teal"          . "#73daca")
    ("tokyo-teal-dark"     . "#41a6b5")
    ("tokyo-cyan"          . "#7dcfff")
    ("tokyo-cyan-bright"   . "#2ac3de")
    ("tokyo-cyan-support"  . "#0db9d7")
    ("tokyo-cyan-pale"     . "#89ddff")
    ("tokyo-cyan-ice"      . "#b4f9f8")
    ("tokyo-blue"          . "#7aa2f7")
    ("tokyo-blue-dark"     . "#3d59a1")
    ("tokyo-blue7"         . "#394b70")
    ("tokyo-magenta"       . "#bb9af7")
    ("tokyo-magenta-dark"  . "#9d7cd8")
    ("tokyo-magenta-hot"   . "#ff007c")

    ;; Comment
    ("tokyo-comment"       . "#565f89")

    ;; Git / Diff
    ("tokyo-git-add"       . "#449dab")
    ("tokyo-git-change"    . "#6183bb")
    ("tokyo-git-delete"    . "#914c54")
    ("tokyo-git-ignored"   . "#515670")
    ("tokyo-diff-add-bg"   . "#1a3a3a")
    ("tokyo-diff-del-bg"   . "#3a2020")
    ("tokyo-diff-chg-bg"   . "#1a2a4a")

    ;; Headings
    ("tokyo-heading1"      . "#89ddff")
    ("tokyo-heading2"      . "#61bdf2")
    ("tokyo-heading3"      . "#7aa2f7")
    ("tokyo-heading4"      . "#6d91de")
    ("tokyo-heading5"      . "#9aa5ce")
    ("tokyo-heading6"      . "#747ca1"))
  "The Tokyo Night (night) color palette.
Darkest background variant.")

(defconst tokyo-night-storm-colors-alist
  '(;; Background shades
    ("tokyo-bg-darkest"    . "#1b1e2d")
    ("tokyo-bg-dark"       . "#1f2335")
    ("tokyo-bg"            . "#24283b")
    ("tokyo-bg-highlight"  . "#292e42")
    ("tokyo-bg-line"       . "#262b3e")

    ;; Foreground shades
    ("tokyo-fg"            . "#c0caf5")
    ("tokyo-fg-dark"       . "#a9b1d6")
    ("tokyo-fg-muted"      . "#9aa5ce")
    ("tokyo-fg-gutter"     . "#3b4261")

    ;; UI elements
    ("tokyo-line-nr"       . "#363b54")
    ("tokyo-line-nr-cur"   . "#787c99")
    ("tokyo-indent"        . "#2c3048")
    ("tokyo-selection"     . "#515c7e")
    ("tokyo-dark3"         . "#545c7e")
    ("tokyo-dark5"         . "#737aa2")
    ("tokyo-terminal-blk"  . "#414868")
    ("tokyo-bracket"       . "#42465d")
    ("tokyo-whitespace"    . "#363b54")

    ;; Core accent colors
    ("tokyo-red"           . "#f7768e")
    ("tokyo-red-dark"      . "#db4b4b")
    ("tokyo-orange"        . "#ff9e64")
    ("tokyo-yellow"        . "#e0af68")
    ("tokyo-green"         . "#9ece6a")
    ("tokyo-teal"          . "#73daca")
    ("tokyo-teal-dark"     . "#41a6b5")
    ("tokyo-cyan"          . "#7dcfff")
    ("tokyo-cyan-bright"   . "#2ac3de")
    ("tokyo-cyan-support"  . "#0db9d7")
    ("tokyo-cyan-pale"     . "#89ddff")
    ("tokyo-cyan-ice"      . "#b4f9f8")
    ("tokyo-blue"          . "#7aa2f7")
    ("tokyo-blue-dark"     . "#3d59a1")
    ("tokyo-blue7"         . "#394b70")
    ("tokyo-magenta"       . "#bb9af7")
    ("tokyo-magenta-dark"  . "#9d7cd8")
    ("tokyo-magenta-hot"   . "#ff007c")

    ;; Comment
    ("tokyo-comment"       . "#565f89")

    ;; Git / Diff
    ("tokyo-git-add"       . "#449dab")
    ("tokyo-git-change"    . "#6183bb")
    ("tokyo-git-delete"    . "#914c54")
    ("tokyo-git-ignored"   . "#515670")
    ("tokyo-diff-add-bg"   . "#1a3a3a")
    ("tokyo-diff-del-bg"   . "#3a2020")
    ("tokyo-diff-chg-bg"   . "#1a2a4a")

    ;; Headings
    ("tokyo-heading1"      . "#89ddff")
    ("tokyo-heading2"      . "#61bdf2")
    ("tokyo-heading3"      . "#7aa2f7")
    ("tokyo-heading4"      . "#6d91de")
    ("tokyo-heading5"      . "#9aa5ce")
    ("tokyo-heading6"      . "#747ca1"))
  "The Tokyo Night (storm) color palette.
Medium background variant, same accents as night.")

(defconst tokyo-night-moon-colors-alist
  '(;; Background shades
    ("tokyo-bg-darkest"    . "#191b29")
    ("tokyo-bg-dark"       . "#1e2030")
    ("tokyo-bg"            . "#222436")
    ("tokyo-bg-highlight"  . "#2f334d")
    ("tokyo-bg-line"       . "#282b41")

    ;; Foreground shades
    ("tokyo-fg"            . "#c8d3f5")
    ("tokyo-fg-dark"       . "#828bb8")
    ("tokyo-fg-muted"      . "#9da8d2")
    ("tokyo-fg-gutter"     . "#3b4261")

    ;; UI elements
    ("tokyo-line-nr"       . "#3b4261")
    ("tokyo-line-nr-cur"   . "#828bb8")
    ("tokyo-indent"        . "#2a2d44")
    ("tokyo-selection"     . "#515c7e")
    ("tokyo-dark3"         . "#545c7e")
    ("tokyo-dark5"         . "#737aa2")
    ("tokyo-terminal-blk"  . "#444a73")
    ("tokyo-bracket"       . "#464b6e")
    ("tokyo-whitespace"    . "#3b4261")

    ;; Core accent colors
    ("tokyo-red"           . "#ff757f")
    ("tokyo-red-dark"      . "#c53b53")
    ("tokyo-orange"        . "#ff966c")
    ("tokyo-yellow"        . "#ffc777")
    ("tokyo-green"         . "#c3e88d")
    ("tokyo-teal"          . "#4fd6be")
    ("tokyo-teal-dark"     . "#41a6b5")
    ("tokyo-cyan"          . "#86e1fc")
    ("tokyo-cyan-bright"   . "#65bcff")
    ("tokyo-cyan-support"  . "#0db9d7")
    ("tokyo-cyan-pale"     . "#89ddff")
    ("tokyo-cyan-ice"      . "#b4f9f8")
    ("tokyo-blue"          . "#82aaff")
    ("tokyo-blue-dark"     . "#3e68d7")
    ("tokyo-blue7"         . "#394b70")
    ("tokyo-magenta"       . "#c099ff")
    ("tokyo-magenta-dark"  . "#fca7ea")
    ("tokyo-magenta-hot"   . "#ff007c")

    ;; Comment
    ("tokyo-comment"       . "#636da6")

    ;; Git / Diff
    ("tokyo-git-add"       . "#b8db87")
    ("tokyo-git-change"    . "#7ca1f2")
    ("tokyo-git-delete"    . "#e26a75")
    ("tokyo-git-ignored"   . "#545c7e")
    ("tokyo-diff-add-bg"   . "#273330")
    ("tokyo-diff-del-bg"   . "#3a2028")
    ("tokyo-diff-chg-bg"   . "#1e2a4a")

    ;; Headings
    ("tokyo-heading1"      . "#89ddff")
    ("tokyo-heading2"      . "#76c4ff")
    ("tokyo-heading3"      . "#82aaff")
    ("tokyo-heading4"      . "#7895d4")
    ("tokyo-heading5"      . "#828bb8")
    ("tokyo-heading6"      . "#737aa2"))
  "The Tokyo Night (moon) color palette.
Blue-tinted dark variant with unique accents.")

(defconst tokyo-night-day-colors-alist
  '(;; Background shades
    ("tokyo-bg-darkest"    . "#c1c9df")
    ("tokyo-bg-dark"       . "#d0d5e3")
    ("tokyo-bg"            . "#e1e2e7")
    ("tokyo-bg-highlight"  . "#c4c8da")
    ("tokyo-bg-line"       . "#d8dae3")

    ;; Foreground shades
    ("tokyo-fg"            . "#3760bf")
    ("tokyo-fg-dark"       . "#6172b0")
    ("tokyo-fg-muted"      . "#506399")
    ("tokyo-fg-gutter"     . "#a8aecb")

    ;; UI elements
    ("tokyo-line-nr"       . "#a8aecb")
    ("tokyo-line-nr-cur"   . "#68709a")
    ("tokyo-indent"        . "#d4d6de")
    ("tokyo-selection"     . "#b6bfe2")
    ("tokyo-dark3"         . "#8990b3")
    ("tokyo-dark5"         . "#68709a")
    ("tokyo-terminal-blk"  . "#a1a6c5")
    ("tokyo-bracket"       . "#b8bcd0")
    ("tokyo-whitespace"    . "#b8bdd0")

    ;; Core accent colors
    ("tokyo-red"           . "#f52a65")
    ("tokyo-red-dark"      . "#c64343")
    ("tokyo-orange"        . "#b15c00")
    ("tokyo-yellow"        . "#8c6c3e")
    ("tokyo-green"         . "#587539")
    ("tokyo-teal"          . "#387068")
    ("tokyo-teal-dark"     . "#38919f")
    ("tokyo-cyan"          . "#007197")
    ("tokyo-cyan-bright"   . "#188092")
    ("tokyo-cyan-support"  . "#07879d")
    ("tokyo-cyan-pale"     . "#006a83")
    ("tokyo-cyan-ice"      . "#2e5857")
    ("tokyo-blue"          . "#2e7de9")
    ("tokyo-blue-dark"     . "#7890dd")
    ("tokyo-blue7"         . "#92a6d5")
    ("tokyo-magenta"       . "#9854f1")
    ("tokyo-magenta-dark"  . "#7847bd")
    ("tokyo-magenta-hot"   . "#d20065")

    ;; Comment
    ("tokyo-comment"       . "#848cb5")

    ;; Git / Diff
    ("tokyo-git-add"       . "#4197a4")
    ("tokyo-git-change"    . "#506d9c")
    ("tokyo-git-delete"    . "#c47981")
    ("tokyo-git-ignored"   . "#8990b3")
    ("tokyo-diff-add-bg"   . "#cee8d0")
    ("tokyo-diff-del-bg"   . "#ecd0d4")
    ("tokyo-diff-chg-bg"   . "#d0dcea")

    ;; Headings
    ("tokyo-heading1"      . "#006a83")
    ("tokyo-heading2"      . "#1a74b0")
    ("tokyo-heading3"      . "#2e7de9")
    ("tokyo-heading4"      . "#5a74b0")
    ("tokyo-heading5"      . "#6172b0")
    ("tokyo-heading6"      . "#68709a"))
  "The Tokyo Night (day) color palette.
Light variant.")

;;; Face Application

(defun tokyo-night--apply-theme (theme-name colors-alist)
  "Apply the Tokyo Night face definitions to THEME-NAME using COLORS-ALIST."
  (let* ((merged (append tokyo-night-override-colors-alist colors-alist))
         (class '((class color) (min-colors 88))))
    (cl-flet ((c (name) (cdr (assoc name merged))))
      (let ((tokyo-bg-darkest    (c "tokyo-bg-darkest"))
            (tokyo-bg-dark       (c "tokyo-bg-dark"))
            (tokyo-bg            (c "tokyo-bg"))
            (tokyo-bg-highlight  (c "tokyo-bg-highlight"))
            (tokyo-bg-line       (c "tokyo-bg-line"))
            (tokyo-fg            (c "tokyo-fg"))
            (tokyo-fg-dark       (c "tokyo-fg-dark"))
            (tokyo-fg-muted      (c "tokyo-fg-muted"))
            (tokyo-fg-gutter     (c "tokyo-fg-gutter"))
            (tokyo-line-nr       (c "tokyo-line-nr"))
            (tokyo-line-nr-cur   (c "tokyo-line-nr-cur"))
            (tokyo-indent        (c "tokyo-indent"))
            (tokyo-selection     (c "tokyo-selection"))
            (tokyo-dark3         (c "tokyo-dark3"))
            (tokyo-dark5         (c "tokyo-dark5"))
            (tokyo-terminal-blk  (c "tokyo-terminal-blk"))
            (tokyo-bracket       (c "tokyo-bracket"))
            (tokyo-whitespace    (c "tokyo-whitespace"))
            (tokyo-red           (c "tokyo-red"))
            (tokyo-red-dark      (c "tokyo-red-dark"))
            (tokyo-orange        (c "tokyo-orange"))
            (tokyo-yellow        (c "tokyo-yellow"))
            (tokyo-green         (c "tokyo-green"))
            (tokyo-teal          (c "tokyo-teal"))
            (tokyo-teal-dark     (c "tokyo-teal-dark"))
            (tokyo-cyan          (c "tokyo-cyan"))
            (tokyo-cyan-bright   (c "tokyo-cyan-bright"))
            (tokyo-cyan-support  (c "tokyo-cyan-support"))
            (tokyo-cyan-pale     (c "tokyo-cyan-pale"))
            (tokyo-cyan-ice      (c "tokyo-cyan-ice"))
            (tokyo-blue          (c "tokyo-blue"))
            (tokyo-blue-dark     (c "tokyo-blue-dark"))
            (tokyo-blue7         (c "tokyo-blue7"))
            (tokyo-magenta       (c "tokyo-magenta"))
            (tokyo-magenta-dark  (c "tokyo-magenta-dark"))
            (tokyo-magenta-hot   (c "tokyo-magenta-hot"))
            (tokyo-comment       (c "tokyo-comment"))
            (tokyo-git-add       (c "tokyo-git-add"))
            (tokyo-git-change    (c "tokyo-git-change"))
            (tokyo-git-delete    (c "tokyo-git-delete"))
            (tokyo-git-ignored   (c "tokyo-git-ignored"))
            (tokyo-diff-add-bg   (c "tokyo-diff-add-bg"))
            (tokyo-diff-del-bg   (c "tokyo-diff-del-bg"))
            (tokyo-diff-chg-bg   (c "tokyo-diff-chg-bg"))
            (tokyo-heading1      (c "tokyo-heading1"))
            (tokyo-heading2      (c "tokyo-heading2"))
            (tokyo-heading3      (c "tokyo-heading3"))
            (tokyo-heading4      (c "tokyo-heading4"))
            (tokyo-heading5      (c "tokyo-heading5"))
            (tokyo-heading6      (c "tokyo-heading6"))
            (h1 (if tokyo-night-scale-headings 1.3 1.0))
            (h2 (if tokyo-night-scale-headings 1.2 1.0))
            (h3 (if tokyo-night-scale-headings 1.1 1.0))
            (h-doc (if tokyo-night-scale-headings 1.4 1.0)))

        (custom-theme-set-faces
         theme-name

;;;; Built-in faces
;;;;; basic coloring
         `(default ((,class (:foreground ,tokyo-fg :background ,tokyo-bg))))
         `(cursor ((,class (:background ,tokyo-fg))))
         `(fringe ((,class (:background ,tokyo-bg :foreground ,tokyo-fg-gutter))))
         `(header-line ((,class (:background ,tokyo-bg-dark :foreground ,tokyo-fg-dark))))
         `(highlight ((,class (:background ,tokyo-bg-highlight))))
         `(success ((,class (:foreground ,tokyo-green :weight bold))))
         `(warning ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(error ((,class (:foreground ,tokyo-red-dark :weight bold))))
         `(link ((,class (:foreground ,tokyo-teal :underline t))))
         `(link-visited ((,class (:foreground ,tokyo-magenta :underline t))))
         `(button ((,class (:foreground ,tokyo-teal :underline t))))
         `(minibuffer-prompt ((,class (:foreground ,tokyo-cyan))))
         `(escape-glyph ((,class (:foreground ,tokyo-cyan-pale))))
         `(homoglyph ((,class (:foreground ,tokyo-cyan-pale))))
         `(tooltip ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-dark))))
         `(menu ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-dark))))
         `(shadow ((,class (:foreground ,tokyo-comment))))
         `(region ((,class (:background ,tokyo-selection :extend t))))
         `(secondary-selection ((,class (:background ,tokyo-bg-highlight :extend t))))
         `(trailing-whitespace ((,class (:background ,tokyo-red))))
         `(vertical-border ((,class (:foreground ,tokyo-terminal-blk))))
         `(window-divider ((,class (:foreground ,tokyo-terminal-blk))))
         `(window-divider-first-pixel ((,class (:foreground ,tokyo-terminal-blk))))
         `(window-divider-last-pixel ((,class (:foreground ,tokyo-terminal-blk))))
         `(widget-field ((,class (:background ,tokyo-bg-highlight :extend t))))

;;;;; fill-column-indicator
         `(fill-column-indicator ((,class (:foreground ,tokyo-indent :weight normal))))

;;;;; mode-line
         `(mode-line ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-dark
                                           :box (:line-width -1 :color ,tokyo-terminal-blk)))))
         `(mode-line-inactive ((,class (:foreground ,tokyo-dark5 :background ,tokyo-bg-darkest
                                                    :box (:line-width -1 :color ,tokyo-bg-dark)))))
         `(mode-line-buffer-id ((,class (:foreground ,tokyo-blue :weight bold))))
         `(mode-line-emphasis ((,class (:foreground ,tokyo-fg :weight bold))))
         `(mode-line-highlight ((,class (:foreground ,tokyo-magenta))))

;;;;; ansi-colors
         `(ansi-color-black ((,class (:foreground ,tokyo-line-nr :background ,tokyo-line-nr))))
         `(ansi-color-red ((,class (:foreground ,tokyo-red :background ,tokyo-red))))
         `(ansi-color-green ((,class (:foreground ,tokyo-teal :background ,tokyo-teal))))
         `(ansi-color-yellow ((,class (:foreground ,tokyo-yellow :background ,tokyo-yellow))))
         `(ansi-color-blue ((,class (:foreground ,tokyo-blue :background ,tokyo-blue))))
         `(ansi-color-magenta ((,class (:foreground ,tokyo-magenta :background ,tokyo-magenta))))
         `(ansi-color-cyan ((,class (:foreground ,tokyo-cyan :background ,tokyo-cyan))))
         `(ansi-color-white ((,class (:foreground ,tokyo-line-nr-cur :background ,tokyo-line-nr-cur))))
         `(ansi-color-bright-black ((,class (:foreground ,tokyo-terminal-blk :background ,tokyo-terminal-blk))))
         `(ansi-color-bright-red ((,class (:foreground ,tokyo-red :background ,tokyo-red))))
         `(ansi-color-bright-green ((,class (:foreground ,tokyo-teal :background ,tokyo-teal))))
         `(ansi-color-bright-yellow ((,class (:foreground ,tokyo-yellow :background ,tokyo-yellow))))
         `(ansi-color-bright-blue ((,class (:foreground ,tokyo-blue :background ,tokyo-blue))))
         `(ansi-color-bright-magenta ((,class (:foreground ,tokyo-magenta :background ,tokyo-magenta))))
         `(ansi-color-bright-cyan ((,class (:foreground ,tokyo-cyan :background ,tokyo-cyan))))
         `(ansi-color-bright-white ((,class (:foreground ,tokyo-fg-dark :background ,tokyo-fg-dark))))

;;;;; font-lock
         `(font-lock-builtin-face ((,class (:foreground ,tokyo-cyan-bright))))
         `(font-lock-comment-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(font-lock-comment-delimiter-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(font-lock-constant-face ((,class (:foreground ,tokyo-orange))))
         `(font-lock-doc-face ((,class (:foreground ,tokyo-comment))))
         `(font-lock-doc-markup-face ((,class (:foreground ,tokyo-dark5))))
         `(font-lock-function-name-face ((,class (:foreground ,tokyo-blue))))
         `(font-lock-function-call-face ((,class (:foreground ,tokyo-blue))))
         `(font-lock-keyword-face ((,class (:foreground ,tokyo-magenta :slant italic))))
         `(font-lock-negation-char-face ((,class (:foreground ,tokyo-cyan-pale))))
         `(font-lock-number-face ((,class (:foreground ,tokyo-orange))))
         `(font-lock-operator-face ((,class (:foreground ,tokyo-cyan-pale))))
         `(font-lock-preprocessor-face ((,class (:foreground ,tokyo-teal))))
         `(font-lock-regexp-grouping-construct ((,class (:foreground ,tokyo-cyan-ice :weight bold))))
         `(font-lock-regexp-grouping-backslash ((,class (:foreground ,tokyo-cyan-ice :weight bold))))
         `(font-lock-string-face ((,class (:foreground ,tokyo-green))))
         `(font-lock-type-face ((,class (:foreground ,tokyo-cyan))))
         `(font-lock-variable-name-face ((,class (:foreground ,tokyo-fg))))
         `(font-lock-variable-use-face ((,class (:foreground ,tokyo-fg))))
         `(font-lock-warning-face ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(font-lock-property-name-face ((,class (:foreground ,tokyo-teal))))
         `(font-lock-property-use-face ((,class (:foreground ,tokyo-teal))))
         `(font-lock-bracket-face ((,class (:foreground ,tokyo-fg-dark))))
         `(font-lock-delimiter-face ((,class (:foreground ,tokyo-fg-dark))))
         `(font-lock-escape-face ((,class (:foreground ,tokyo-cyan-pale))))
         `(font-lock-misc-punctuation-face ((,class (:foreground ,tokyo-cyan-pale))))

;;;;; line numbers
         `(line-number ((,class (:foreground ,tokyo-line-nr :background ,tokyo-bg))))
         `(line-number-current-line ((,class (:foreground ,tokyo-line-nr-cur :background ,tokyo-bg :weight bold))))
         `(line-number-major-tick ((,class (:foreground ,tokyo-dark5 :background ,tokyo-bg))))
         `(line-number-minor-tick ((,class (:foreground ,tokyo-fg-gutter :background ,tokyo-bg))))

;;;;; isearch / replace
         `(isearch ((,class (:foreground ,tokyo-bg :background ,tokyo-magenta-hot :weight bold))))
         `(isearch-fail ((,class (:foreground ,tokyo-red-dark :background ,tokyo-bg-highlight))))
         `(isearch-group-1 ((,class (:foreground ,tokyo-bg :background ,tokyo-blue))))
         `(isearch-group-2 ((,class (:foreground ,tokyo-bg :background ,tokyo-teal))))
         `(lazy-highlight ((,class (:foreground ,tokyo-fg :background ,tokyo-blue-dark))))
         `(match ((,class (:foreground ,tokyo-green :background ,tokyo-bg :weight bold))))
         `(query-replace ((,class (:foreground ,tokyo-bg :background ,tokyo-orange :weight bold))))

;;;;; show-paren
         `(show-paren-match ((,class (:foreground ,tokyo-cyan-pale :background ,tokyo-bracket :weight bold))))
         `(show-paren-match-expression ((,class (:background ,tokyo-bg-highlight))))
         `(show-paren-mismatch ((,class (:foreground ,tokyo-red :background ,tokyo-bg-highlight :weight bold :underline t))))

;;;;; completions
         `(completions-annotations ((,class (:foreground ,tokyo-comment))))
         `(completions-common-part ((,class (:foreground ,tokyo-blue :weight bold))))
         `(completions-first-difference ((,class (:foreground ,tokyo-orange))))
         `(completions-highlight ((,class (:background ,tokyo-bg-highlight))))
         `(completions-group-title ((,class (:foreground ,tokyo-magenta :weight bold :slant italic))))
         `(completions-group-separator ((,class (:foreground ,tokyo-comment :strike-through t))))

;;;;; compilation
         `(compilation-error ((,class (:foreground ,tokyo-red-dark :weight bold))))
         `(compilation-warning ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(compilation-info ((,class (:foreground ,tokyo-teal))))
         `(compilation-mode-line-exit ((,class (:foreground ,tokyo-green :weight bold))))
         `(compilation-mode-line-fail ((,class (:foreground ,tokyo-red :weight bold))))
         `(compilation-mode-line-run ((,class (:foreground ,tokyo-blue :weight bold))))
         `(compilation-line-number ((,class (:foreground ,tokyo-dark5))))
         `(compilation-column-number ((,class (:foreground ,tokyo-dark5))))

;;;;; customize
         `(custom-variable-tag ((,class (:foreground ,tokyo-blue :weight bold))))
         `(custom-group-tag ((,class (:foreground ,tokyo-blue :weight bold :height 1.2))))
         `(custom-group-tag-1 ((,class (:foreground ,tokyo-magenta :weight bold :height 1.2))))
         `(custom-state ((,class (:foreground ,tokyo-green))))
         `(custom-button ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-highlight
                                               :box (:line-width 2 :color ,tokyo-terminal-blk :style released-button)))))
         `(custom-button-mouse ((,class (:foreground ,tokyo-fg :background ,tokyo-selection
                                                     :box (:line-width 2 :color ,tokyo-terminal-blk :style released-button)))))
         `(custom-button-pressed ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-highlight
                                                       :box (:line-width 2 :color ,tokyo-terminal-blk :style pressed-button)))))

;;;;; diff
         `(diff-added ((,class (:foreground ,tokyo-git-add :background ,tokyo-diff-add-bg :extend t))))
         `(diff-removed ((,class (:foreground ,tokyo-git-delete :background ,tokyo-diff-del-bg :extend t))))
         `(diff-changed ((,class (:foreground ,tokyo-git-change :background ,tokyo-diff-chg-bg :extend t))))
         `(diff-refine-added ((,class (:foreground ,tokyo-green :background ,tokyo-diff-add-bg :weight bold :extend t))))
         `(diff-refine-removed ((,class (:foreground ,tokyo-red :background ,tokyo-diff-del-bg :weight bold :extend t))))
         `(diff-refine-changed ((,class (:foreground ,tokyo-blue :background ,tokyo-diff-chg-bg :weight bold :extend t))))
         `(diff-header ((,class (:foreground ,tokyo-fg-dark :background ,tokyo-bg-dark :extend t))))
         `(diff-file-header ((,class (:foreground ,tokyo-blue :background ,tokyo-bg-dark :weight bold :extend t))))
         `(diff-hunk-header ((,class (:foreground ,tokyo-magenta :background ,tokyo-bg-dark :extend t))))
         `(diff-indicator-added ((,class (:foreground ,tokyo-git-add))))
         `(diff-indicator-removed ((,class (:foreground ,tokyo-git-delete))))
         `(diff-indicator-changed ((,class (:foreground ,tokyo-git-change))))
         `(diff-nonexistent ((,class (:foreground ,tokyo-comment))))

;;;;; dired
         `(dired-directory ((,class (:foreground ,tokyo-blue :weight bold))))
         `(dired-flagged ((,class (:foreground ,tokyo-red))))
         `(dired-header ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(dired-ignored ((,class (:foreground ,tokyo-comment))))
         `(dired-mark ((,class (:foreground ,tokyo-orange :weight bold))))
         `(dired-marked ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(dired-perm-write ((,class (:foreground ,tokyo-fg-dark))))
         `(dired-symlink ((,class (:foreground ,tokyo-cyan))))
         `(dired-warning ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(dired-broken-symlink ((,class (:foreground ,tokyo-red :weight bold))))

;;;;; ediff
         `(ediff-current-diff-A ((,class (:background ,tokyo-diff-del-bg :extend t))))
         `(ediff-current-diff-B ((,class (:background ,tokyo-diff-add-bg :extend t))))
         `(ediff-current-diff-C ((,class (:background ,tokyo-diff-chg-bg :extend t))))
         `(ediff-fine-diff-A ((,class (:foreground ,tokyo-red :background ,tokyo-diff-del-bg :weight bold :extend t))))
         `(ediff-fine-diff-B ((,class (:foreground ,tokyo-green :background ,tokyo-diff-add-bg :weight bold :extend t))))
         `(ediff-fine-diff-C ((,class (:foreground ,tokyo-blue :background ,tokyo-diff-chg-bg :weight bold :extend t))))
         `(ediff-even-diff-A ((,class (:background ,tokyo-bg-highlight :extend t))))
         `(ediff-even-diff-B ((,class (:background ,tokyo-bg-highlight :extend t))))
         `(ediff-even-diff-C ((,class (:background ,tokyo-bg-highlight :extend t))))
         `(ediff-odd-diff-A ((,class (:background ,tokyo-bg-highlight :extend t))))
         `(ediff-odd-diff-B ((,class (:background ,tokyo-bg-highlight :extend t))))
         `(ediff-odd-diff-C ((,class (:background ,tokyo-bg-highlight :extend t))))

;;;;; eshell
         `(eshell-prompt ((,class (:foreground ,tokyo-blue :weight bold))))
         `(eshell-ls-archive ((,class (:foreground ,tokyo-magenta))))
         `(eshell-ls-backup ((,class (:foreground ,tokyo-comment))))
         `(eshell-ls-clutter ((,class (:foreground ,tokyo-comment))))
         `(eshell-ls-directory ((,class (:foreground ,tokyo-blue :weight bold))))
         `(eshell-ls-executable ((,class (:foreground ,tokyo-green))))
         `(eshell-ls-missing ((,class (:foreground ,tokyo-red))))
         `(eshell-ls-product ((,class (:foreground ,tokyo-fg-dark))))
         `(eshell-ls-readonly ((,class (:foreground ,tokyo-dark5))))
         `(eshell-ls-special ((,class (:foreground ,tokyo-orange :weight bold))))
         `(eshell-ls-symlink ((,class (:foreground ,tokyo-cyan))))
         `(eshell-ls-unreadable ((,class (:foreground ,tokyo-comment))))

;;;;; erc
         `(erc-action-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(erc-bold-face ((,class (:weight bold))))
         `(erc-current-nick-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(erc-default-face ((,class (:foreground ,tokyo-fg))))
         `(erc-direct-msg-face ((,class (:foreground ,tokyo-orange))))
         `(erc-error-face ((,class (:foreground ,tokyo-red-dark :weight bold))))
         `(erc-fool-face ((,class (:foreground ,tokyo-comment))))
         `(erc-highlight-face ((,class (:background ,tokyo-bg-highlight))))
         `(erc-input-face ((,class (:foreground ,tokyo-fg))))
         `(erc-keyword-face ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(erc-my-nick-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(erc-nick-default-face ((,class (:foreground ,tokyo-teal :weight bold))))
         `(erc-nick-msg-face ((,class (:foreground ,tokyo-orange :weight bold))))
         `(erc-notice-face ((,class (:foreground ,tokyo-comment))))
         `(erc-pal-face ((,class (:foreground ,tokyo-green :weight bold))))
         `(erc-prompt-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(erc-timestamp-face ((,class (:foreground ,tokyo-dark5))))
         `(erc-underline-face ((,class (:underline t))))

;;;;; flymake
         `(flymake-error ((,class (:underline (:style wave :color ,tokyo-red-dark)))))
         `(flymake-warning ((,class (:underline (:style wave :color ,tokyo-yellow)))))
         `(flymake-note ((,class (:underline (:style wave :color ,tokyo-teal)))))

;;;;; flyspell
         `(flyspell-duplicate ((,class (:underline (:style wave :color ,tokyo-yellow)))))
         `(flyspell-incorrect ((,class (:underline (:style wave :color ,tokyo-red-dark)))))

;;;;; gnus
         `(gnus-group-mail-1 ((,class (:foreground ,tokyo-blue :weight bold))))
         `(gnus-group-mail-1-empty ((,class (:foreground ,tokyo-blue))))
         `(gnus-group-mail-2 ((,class (:foreground ,tokyo-cyan :weight bold))))
         `(gnus-group-mail-2-empty ((,class (:foreground ,tokyo-cyan))))
         `(gnus-group-mail-3 ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(gnus-group-mail-3-empty ((,class (:foreground ,tokyo-magenta))))
         `(gnus-group-mail-low ((,class (:foreground ,tokyo-comment :weight bold))))
         `(gnus-group-mail-low-empty ((,class (:foreground ,tokyo-comment))))
         `(gnus-group-news-1 ((,class (:foreground ,tokyo-blue :weight bold))))
         `(gnus-group-news-1-empty ((,class (:foreground ,tokyo-blue))))
         `(gnus-group-news-2 ((,class (:foreground ,tokyo-cyan :weight bold))))
         `(gnus-group-news-2-empty ((,class (:foreground ,tokyo-cyan))))
         `(gnus-group-news-low ((,class (:foreground ,tokyo-comment :weight bold))))
         `(gnus-group-news-low-empty ((,class (:foreground ,tokyo-comment))))
         `(gnus-header-content ((,class (:foreground ,tokyo-fg-dark))))
         `(gnus-header-from ((,class (:foreground ,tokyo-blue :weight bold))))
         `(gnus-header-name ((,class (:foreground ,tokyo-magenta))))
         `(gnus-header-newsgroups ((,class (:foreground ,tokyo-teal :weight bold))))
         `(gnus-header-subject ((,class (:foreground ,tokyo-fg :weight bold))))
         `(gnus-summary-cancelled ((,class (:foreground ,tokyo-red :background ,tokyo-bg))))
         `(gnus-summary-normal-ancient ((,class (:foreground ,tokyo-comment))))
         `(gnus-summary-normal-read ((,class (:foreground ,tokyo-dark5))))
         `(gnus-summary-normal-ticked ((,class (:foreground ,tokyo-fg-dark :slant italic))))
         `(gnus-summary-normal-unread ((,class (:foreground ,tokyo-fg :weight bold))))
         `(gnus-summary-selected ((,class (:foreground ,tokyo-blue :weight bold :underline t))))

;;;;; grep
         `(grep-context-face ((,class (:foreground ,tokyo-fg-dark))))
         `(grep-error-face ((,class (:foreground ,tokyo-red-dark :weight bold :underline t))))
         `(grep-hit-face ((,class (:foreground ,tokyo-blue))))
         `(grep-match-face ((,class (:foreground ,tokyo-orange :weight bold))))

;;;;; hi-lock
         `(hi-blue ((,class (:foreground ,tokyo-bg :background ,tokyo-blue))))
         `(hi-green ((,class (:foreground ,tokyo-bg :background ,tokyo-green))))
         `(hi-pink ((,class (:foreground ,tokyo-bg :background ,tokyo-magenta-hot))))
         `(hi-yellow ((,class (:foreground ,tokyo-bg :background ,tokyo-yellow))))
         `(hi-blue-b ((,class (:foreground ,tokyo-blue :weight bold))))
         `(hi-green-b ((,class (:foreground ,tokyo-green :weight bold))))
         `(hi-red-b ((,class (:foreground ,tokyo-red :weight bold))))

;;;;; hl-line
         `(hl-line ((,class (:background ,tokyo-bg-line :extend t))))

;;;;; hl-todo
         `(hl-todo ((,class (:foreground ,tokyo-magenta-hot :weight bold))))

;;;;; icomplete
         `(icomplete-first-match ((,class (:foreground ,tokyo-green :weight bold))))
         `(icomplete-selected-match ((,class (:background ,tokyo-bg-highlight))))

;;;;; ido
         `(ido-first-match ((,class (:foreground ,tokyo-green :weight bold))))
         `(ido-only-match ((,class (:foreground ,tokyo-teal :weight bold))))
         `(ido-subdir ((,class (:foreground ,tokyo-blue))))
         `(ido-incomplete-regexp ((,class (:foreground ,tokyo-red-dark))))
         `(ido-indicator ((,class (:foreground ,tokyo-yellow :background ,tokyo-bg))))
         `(ido-virtual ((,class (:foreground ,tokyo-comment))))

;;;;; info
         `(Info-quoted ((,class (:foreground ,tokyo-orange :inherit fixed-pitch-serif))))
         `(info-header-node ((,class (:foreground ,tokyo-blue :weight bold))))
         `(info-header-xref ((,class (:foreground ,tokyo-teal))))
         `(info-menu-header ((,class (:foreground ,tokyo-fg :weight bold))))
         `(info-menu-star ((,class (:foreground ,tokyo-red))))
         `(info-node ((,class (:foreground ,tokyo-blue :weight bold))))
         `(info-title-1 ((,class (:foreground ,tokyo-heading1 :weight bold :height ,h1))))
         `(info-title-2 ((,class (:foreground ,tokyo-heading2 :weight bold :height ,h2))))
         `(info-title-3 ((,class (:foreground ,tokyo-heading3 :weight bold :height ,h3))))
         `(info-title-4 ((,class (:foreground ,tokyo-heading4 :weight bold))))
         `(info-xref ((,class (:foreground ,tokyo-teal :underline t))))
         `(info-xref-visited ((,class (:foreground ,tokyo-magenta :underline t))))

;;;;; message (email composition)
         `(message-cited-text-1 ((,class (:foreground ,tokyo-teal))))
         `(message-cited-text-2 ((,class (:foreground ,tokyo-green))))
         `(message-cited-text-3 ((,class (:foreground ,tokyo-comment))))
         `(message-cited-text-4 ((,class (:foreground ,tokyo-dark5))))
         `(message-header-cc ((,class (:foreground ,tokyo-blue))))
         `(message-header-name ((,class (:foreground ,tokyo-magenta))))
         `(message-header-newsgroups ((,class (:foreground ,tokyo-teal :weight bold))))
         `(message-header-other ((,class (:foreground ,tokyo-fg-dark))))
         `(message-header-subject ((,class (:foreground ,tokyo-fg :weight bold))))
         `(message-header-to ((,class (:foreground ,tokyo-blue :weight bold))))
         `(message-header-xheader ((,class (:foreground ,tokyo-dark5))))
         `(message-mml ((,class (:foreground ,tokyo-green))))
         `(message-separator ((,class (:foreground ,tokyo-comment))))

;;;;; mu4e
         ;; Headers view – message flags
         `(mu4e-unread-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(mu4e-header-face ((,class (:foreground ,tokyo-fg-dark))))
         `(mu4e-flagged-face ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(mu4e-draft-face ((,class (:foreground ,tokyo-orange :slant italic))))
         `(mu4e-trashed-face ((,class (:foreground ,tokyo-comment :strike-through t))))
         `(mu4e-replied-face ((,class (:foreground ,tokyo-dark5))))
         `(mu4e-forwarded-face ((,class (:foreground ,tokyo-dark5))))
         `(mu4e-related-face ((,class (:foreground ,tokyo-comment :slant italic))))
         ;; Headers view – UI elements
         `(mu4e-header-title-face ((,class (:foreground ,tokyo-fg :weight bold))))
         `(mu4e-header-highlight-face ((,class (:background ,tokyo-bg-highlight :extend t))))
         `(mu4e-header-marks-face ((,class (:foreground ,tokyo-orange :weight bold))))
         `(mu4e-header-key-face ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(mu4e-header-field-face ((,class (:foreground ,tokyo-magenta))))
         `(mu4e-header-value-face ((,class (:foreground ,tokyo-fg-dark))))
         `(mu4e-special-header-value-face ((,class (:foreground ,tokyo-cyan))))
         ;; Message view
         `(mu4e-link-face ((,class (:foreground ,tokyo-teal :underline t))))
         `(mu4e-contact-face ((,class (:foreground ,tokyo-blue))))
         `(mu4e-highlight-face ((,class (:foreground ,tokyo-cyan-bright :weight bold))))
         `(mu4e-title-face ((,class (:foreground ,tokyo-fg :weight bold))))
         `(mu4e-url-number-face ((,class (:foreground ,tokyo-teal-dark :weight bold))))
         `(mu4e-footer-face ((,class (:foreground ,tokyo-comment :slant italic))))
         ;; Compose
         `(mu4e-compose-separator-face ((,class (:foreground ,tokyo-comment))))
         ;; System / mode-line
         `(mu4e-modeline-face ((,class (:foreground ,tokyo-blue))))
         `(mu4e-system-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(mu4e-ok-face ((,class (:foreground ,tokyo-green :weight bold))))
         `(mu4e-warning-face ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(mu4e-region-code ((,class (:background ,tokyo-bg-highlight))))
         ;; Threading
         `(mu4e-thread-fold-face ((,class (:foreground ,tokyo-comment :slant italic))))

;;;;; notmuch
         ;; Search view
         `(notmuch-search-date ((,class (:foreground ,tokyo-dark5))))
         `(notmuch-search-count ((,class (:foreground ,tokyo-comment))))
         `(notmuch-search-subject ((,class (:foreground ,tokyo-fg))))
         `(notmuch-search-matching-authors ((,class (:foreground ,tokyo-blue))))
         `(notmuch-search-non-matching-authors ((,class (:foreground ,tokyo-comment))))
         `(notmuch-search-flagged ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(notmuch-search-unread ((,class (:foreground ,tokyo-fg :weight bold))))
         ;; Tree view
         `(notmuch-tree-match-author-face ((,class (:foreground ,tokyo-blue))))
         `(notmuch-tree-match-date-face ((,class (:foreground ,tokyo-dark5))))
         `(notmuch-tree-match-subject-face ((,class (:foreground ,tokyo-fg))))
         `(notmuch-tree-match-tree-face ((,class (:foreground ,tokyo-comment))))
         `(notmuch-tree-match-tag-face ((,class (:foreground ,tokyo-teal))))
         `(notmuch-tree-no-match-author-face ((,class (:foreground ,tokyo-comment))))
         `(notmuch-tree-no-match-date-face ((,class (:foreground ,tokyo-comment))))
         `(notmuch-tree-no-match-subject-face ((,class (:foreground ,tokyo-comment))))
         `(notmuch-tree-no-match-tree-face ((,class (:foreground ,tokyo-fg-gutter))))
         `(notmuch-tree-no-match-tag-face ((,class (:foreground ,tokyo-comment))))
         ;; Show view
         `(notmuch-message-summary-face ((,class (:foreground ,tokyo-fg-dark :background ,tokyo-bg-dark))))
         ;; Tags
         `(notmuch-tag-face ((,class (:foreground ,tokyo-teal))))
         `(notmuch-tag-unread ((,class (:foreground ,tokyo-blue :weight bold))))
         `(notmuch-tag-flagged ((,class (:foreground ,tokyo-yellow))))
         `(notmuch-tag-deleted ((,class (:foreground ,tokyo-red :strike-through t))))
         `(notmuch-tag-added ((,class (:foreground ,tokyo-green :underline t))))
         ;; Wash (inline content toggling / citations)
         `(notmuch-wash-toggle-button ((,class (:foreground ,tokyo-dark5 :background ,tokyo-bg-dark))))
         `(notmuch-wash-cited-text ((,class (:foreground ,tokyo-comment))))
         ;; Crypto
         `(notmuch-crypto-part-header ((,class (:foreground ,tokyo-fg-dark))))
         `(notmuch-crypto-signature-good ((,class (:foreground ,tokyo-green :weight bold))))
         `(notmuch-crypto-signature-good-key ((,class (:foreground ,tokyo-green))))
         `(notmuch-crypto-signature-bad ((,class (:foreground ,tokyo-red :weight bold))))
         `(notmuch-crypto-signature-unknown ((,class (:foreground ,tokyo-yellow))))
         `(notmuch-crypto-decryption ((,class (:foreground ,tokyo-cyan))))
         ;; Hello (welcome screen)
         `(notmuch-hello-logo-background ((,class (:background ,tokyo-bg-dark))))

;;;;; org-mode
         `(org-archived ((,class (:foreground ,tokyo-comment))))
         `(org-block ((,class (:background ,tokyo-bg-dark :extend t))))
         `(org-block-begin-line ((,class (:foreground ,tokyo-comment :background ,tokyo-bg-dark :extend t :slant italic))))
         `(org-block-end-line ((,class (:inherit org-block-begin-line))))
         `(org-checkbox ((,class (:foreground ,tokyo-blue :weight bold))))
         `(org-checkbox-statistics-done ((,class (:foreground ,tokyo-green))))
         `(org-checkbox-statistics-todo ((,class (:foreground ,tokyo-orange))))
         `(org-code ((,class (:foreground ,tokyo-teal))))
         `(org-date ((,class (:foreground ,tokyo-cyan :underline t))))
         `(org-document-info ((,class (:foreground ,tokyo-fg-dark))))
         `(org-document-info-keyword ((,class (:foreground ,tokyo-comment))))
         `(org-document-title ((,class (:foreground ,tokyo-fg :weight bold :height ,h-doc))))
         `(org-done ((,class (:foreground ,tokyo-green :weight bold))))
         `(org-drawer ((,class (:foreground ,tokyo-comment))))
         `(org-ellipsis ((,class (:foreground ,tokyo-comment :underline nil))))
         `(org-footnote ((,class (:foreground ,tokyo-teal))))
         `(org-formula ((,class (:foreground ,tokyo-orange))))
         `(org-headline-done ((,class (:foreground ,tokyo-comment))))
         `(org-hide ((,class (:foreground ,tokyo-bg))))
         `(org-level-1 ((,class (:inherit outline-1))))
         `(org-level-2 ((,class (:inherit outline-2))))
         `(org-level-3 ((,class (:inherit outline-3))))
         `(org-level-4 ((,class (:inherit outline-4))))
         `(org-level-5 ((,class (:inherit outline-5))))
         `(org-level-6 ((,class (:inherit outline-6))))
         `(org-level-7 ((,class (:inherit outline-7))))
         `(org-level-8 ((,class (:inherit outline-8))))
         `(org-link ((,class (:foreground ,tokyo-teal :underline t))))
         `(org-meta-line ((,class (:foreground ,tokyo-comment))))
         `(org-priority ((,class (:foreground ,tokyo-orange))))
         `(org-property-value ((,class (:foreground ,tokyo-fg-dark))))
         `(org-quote ((,class (:foreground ,tokyo-fg-muted :slant italic :extend t))))
         `(org-scheduled ((,class (:foreground ,tokyo-green))))
         `(org-scheduled-previously ((,class (:foreground ,tokyo-orange))))
         `(org-scheduled-today ((,class (:foreground ,tokyo-green))))
         `(org-special-keyword ((,class (:foreground ,tokyo-comment))))
         `(org-table ((,class (:foreground ,tokyo-fg-muted))))
         `(org-tag ((,class (:foreground ,tokyo-comment :weight normal))))
         `(org-target ((,class (:underline t))))
         `(org-time-grid ((,class (:foreground ,tokyo-dark5))))
         `(org-todo ((,class (:foreground ,tokyo-orange :weight bold))))
         `(org-upcoming-deadline ((,class (:foreground ,tokyo-red))))
         `(org-verbatim ((,class (:foreground ,tokyo-green))))
         `(org-verse ((,class (:inherit org-quote))))
         `(org-warning ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(org-agenda-date ((,class (:foreground ,tokyo-blue))))
         `(org-agenda-date-today ((,class (:foreground ,tokyo-blue :weight bold :slant italic))))
         `(org-agenda-date-weekend ((,class (:foreground ,tokyo-comment))))
         `(org-agenda-date-weekend-today ((,class (:foreground ,tokyo-comment :weight bold))))
         `(org-agenda-done ((,class (:foreground ,tokyo-green))))
         `(org-agenda-structure ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(org-agenda-current-time ((,class (:foreground ,tokyo-cyan))))

;;;;; outline
         `(outline-1 ((,class (:foreground ,tokyo-heading1 :weight bold :height ,h1))))
         `(outline-2 ((,class (:foreground ,tokyo-heading2 :weight bold :height ,h2))))
         `(outline-3 ((,class (:foreground ,tokyo-heading3 :weight bold :height ,h3))))
         `(outline-4 ((,class (:foreground ,tokyo-heading4 :weight bold))))
         `(outline-5 ((,class (:foreground ,tokyo-heading5 :weight bold))))
         `(outline-6 ((,class (:foreground ,tokyo-heading6 :weight bold))))
         `(outline-7 ((,class (:foreground ,tokyo-fg-dark :weight bold))))
         `(outline-8 ((,class (:foreground ,tokyo-dark5 :weight bold))))

;;;;; re-builder
         `(reb-match-0 ((,class (:foreground ,tokyo-bg :background ,tokyo-blue))))
         `(reb-match-1 ((,class (:foreground ,tokyo-bg :background ,tokyo-teal))))
         `(reb-match-2 ((,class (:foreground ,tokyo-bg :background ,tokyo-magenta))))
         `(reb-match-3 ((,class (:foreground ,tokyo-bg :background ,tokyo-orange))))

;;;;; ruler-mode
         `(ruler-mode-default ((,class (:foreground ,tokyo-comment :background ,tokyo-bg-dark))))
         `(ruler-mode-column-number ((,class (:foreground ,tokyo-fg-dark))))
         `(ruler-mode-current-column ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(ruler-mode-fill-column ((,class (:foreground ,tokyo-red))))

;;;;; sh-mode
         `(sh-heredoc ((,class (:foreground ,tokyo-green :slant italic))))
         `(sh-quoted-exec ((,class (:foreground ,tokyo-orange))))

;;;;; shr (eww/elfeed HTML rendering)
         `(shr-h1 ((,class (:foreground ,tokyo-heading1 :weight bold :height ,h1))))
         `(shr-h2 ((,class (:foreground ,tokyo-heading2 :weight bold :height ,h2))))
         `(shr-h3 ((,class (:foreground ,tokyo-heading3 :weight bold :height ,h3))))
         `(shr-h4 ((,class (:foreground ,tokyo-heading4 :weight bold))))
         `(shr-h5 ((,class (:foreground ,tokyo-heading5 :weight bold))))
         `(shr-h6 ((,class (:foreground ,tokyo-heading6 :weight bold))))
         `(shr-link ((,class (:foreground ,tokyo-teal :underline t))))
         `(shr-selected-link ((,class (:foreground ,tokyo-orange :underline t))))
         `(shr-code ((,class (:foreground ,tokyo-teal :background ,tokyo-bg-dark))))
         `(shr-mark ((,class (:foreground ,tokyo-bg :background ,tokyo-yellow))))

;;;;; tab-bar / tab-line
         `(tab-bar ((,class (:foreground ,tokyo-fg-dark :background ,tokyo-bg-darkest))))
         `(tab-bar-tab ((,class (:foreground ,tokyo-fg :background ,tokyo-bg :weight bold))))
         `(tab-bar-tab-inactive ((,class (:foreground ,tokyo-dark5 :background ,tokyo-bg-dark))))
         `(tab-bar-tab-ungrouped ((,class (:foreground ,tokyo-comment :background ,tokyo-bg-dark))))
         `(tab-bar-tab-group-current ((,class (:foreground ,tokyo-blue :background ,tokyo-bg :weight bold))))
         `(tab-bar-tab-group-inactive ((,class (:foreground ,tokyo-dark5 :background ,tokyo-bg-dark))))
         `(tab-line ((,class (:foreground ,tokyo-fg-dark :background ,tokyo-bg-darkest))))
         `(tab-line-tab ((,class (:foreground ,tokyo-fg :background ,tokyo-bg :weight bold))))
         `(tab-line-tab-current ((,class (:foreground ,tokyo-fg :background ,tokyo-bg :weight bold))))
         `(tab-line-tab-inactive ((,class (:foreground ,tokyo-dark5 :background ,tokyo-bg-dark))))
         `(tab-line-highlight ((,class (:background ,tokyo-bg-highlight))))

;;;;; term / ansi-term / vterm
         `(term ((,class (:foreground ,tokyo-fg :background ,tokyo-bg))))
         `(term-color-black ((,class (:foreground ,tokyo-line-nr :background ,tokyo-line-nr))))
         `(term-color-red ((,class (:foreground ,tokyo-red :background ,tokyo-red))))
         `(term-color-green ((,class (:foreground ,tokyo-teal :background ,tokyo-teal))))
         `(term-color-yellow ((,class (:foreground ,tokyo-yellow :background ,tokyo-yellow))))
         `(term-color-blue ((,class (:foreground ,tokyo-blue :background ,tokyo-blue))))
         `(term-color-magenta ((,class (:foreground ,tokyo-magenta :background ,tokyo-magenta))))
         `(term-color-cyan ((,class (:foreground ,tokyo-cyan :background ,tokyo-cyan))))
         `(term-color-white ((,class (:foreground ,tokyo-line-nr-cur :background ,tokyo-line-nr-cur))))

;;;;; whitespace-mode
         `(whitespace-empty ((,class (:foreground ,tokyo-red :background ,tokyo-bg))))
         `(whitespace-hspace ((,class (:foreground ,tokyo-whitespace))))
         `(whitespace-indentation ((,class (:foreground ,tokyo-whitespace))))
         `(whitespace-line ((,class (:foreground ,tokyo-red :background ,tokyo-bg-highlight))))
         `(whitespace-newline ((,class (:foreground ,tokyo-whitespace))))
         `(whitespace-space ((,class (:foreground ,tokyo-whitespace))))
         `(whitespace-space-after-tab ((,class (:foreground ,tokyo-whitespace))))
         `(whitespace-space-before-tab ((,class (:foreground ,tokyo-orange))))
         `(whitespace-tab ((,class (:foreground ,tokyo-whitespace))))
         `(whitespace-trailing ((,class (:foreground ,tokyo-red :background ,tokyo-diff-del-bg))))
         `(whitespace-big-indent ((,class (:foreground ,tokyo-orange :background ,tokyo-bg-highlight))))

;;;;; woman
         `(woman-bold ((,class (:foreground ,tokyo-blue :weight bold))))
         `(woman-italic ((,class (:foreground ,tokyo-magenta :slant italic))))

;;;;; xref
         `(xref-file-header ((,class (:foreground ,tokyo-blue :weight bold))))
         `(xref-line-number ((,class (:foreground ,tokyo-dark5))))
         `(xref-match ((,class (:foreground ,tokyo-orange :weight bold))))

;;;; Built-in packages
;;;;; bookmark
         `(bookmark-face ((,class (:foreground ,tokyo-yellow :background ,tokyo-bg))))

;;;;; calendar
         `(calendar-today ((,class (:foreground ,tokyo-blue :weight bold :underline t))))
         `(calendar-weekend-header ((,class (:foreground ,tokyo-red))))
         `(calendar-weekday-header ((,class (:foreground ,tokyo-teal))))
         `(calendar-month-header ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(holiday ((,class (:foreground ,tokyo-orange))))
         `(diary ((,class (:foreground ,tokyo-yellow))))

;;;;; eglot
         `(eglot-highlight-symbol-face ((,class (:background ,tokyo-bg-highlight :weight bold))))
         `(eglot-diagnostic-tag-unnecessary-face ((,class (:foreground ,tokyo-comment :underline (:style wave :color ,tokyo-dark5)))))
         `(eglot-diagnostic-tag-deprecated-face ((,class (:foreground ,tokyo-comment :strike-through ,tokyo-dark5))))
         `(eglot-inlay-hint-face ((,class (:foreground ,tokyo-dark5 :height 0.9))))

;;;;; eldoc
         `(eldoc-highlight-function-argument ((,class (:foreground ,tokyo-yellow :weight bold))))

;;;;; epa (EasyPG)
         `(epa-field-body ((,class (:foreground ,tokyo-fg-dark :slant italic))))
         `(epa-field-name ((,class (:foreground ,tokyo-blue :weight bold))))
         `(epa-mark ((,class (:foreground ,tokyo-orange :weight bold))))
         `(epa-string ((,class (:foreground ,tokyo-green))))
         `(epa-validity-disabled ((,class (:foreground ,tokyo-red-dark :slant italic))))
         `(epa-validity-high ((,class (:foreground ,tokyo-green :weight bold))))
         `(epa-validity-low ((,class (:foreground ,tokyo-comment))))
         `(epa-validity-medium ((,class (:foreground ,tokyo-yellow))))

;;;;; eww
         `(eww-invalid-certificate ((,class (:foreground ,tokyo-red-dark :weight bold))))
         `(eww-valid-certificate ((,class (:foreground ,tokyo-green :weight bold))))
         `(eww-form-text ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-highlight
                                               :box (:line-width -1 :color ,tokyo-terminal-blk)))))
         `(eww-form-textarea ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-highlight))))
         `(eww-form-checkbox ((,class (:foreground ,tokyo-blue :weight bold))))
         `(eww-form-select ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-highlight
                                                 :box (:line-width -1 :color ,tokyo-terminal-blk)))))
         `(eww-form-submit ((,class (:foreground ,tokyo-fg :background ,tokyo-terminal-blk
                                                 :box (:line-width -1 :color ,tokyo-dark5)))))

;;;;; man
         `(Man-overstrike ((,class (:foreground ,tokyo-blue :weight bold))))
         `(Man-underline ((,class (:foreground ,tokyo-teal :underline t))))
         `(Man-reverse ((,class (:foreground ,tokyo-bg :background ,tokyo-fg))))

;;;;; proced
         `(proced-mark ((,class (:foreground ,tokyo-orange :weight bold))))
         `(proced-marked ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(proced-sort-header ((,class (:foreground ,tokyo-blue :weight bold :underline t))))

;;;;; pulse
         `(pulse-highlight-start-face ((,class (:background ,tokyo-blue-dark))))

;;;;; speedbar
         `(speedbar-button-face ((,class (:foreground ,tokyo-green))))
         `(speedbar-directory-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(speedbar-file-face ((,class (:foreground ,tokyo-fg))))
         `(speedbar-highlight-face ((,class (:background ,tokyo-bg-highlight))))
         `(speedbar-selected-face ((,class (:foreground ,tokyo-orange :weight bold))))
         `(speedbar-separator-face ((,class (:foreground ,tokyo-comment :background ,tokyo-bg-dark))))
         `(speedbar-tag-face ((,class (:foreground ,tokyo-teal))))

;;;;; vc
         `(vc-state-base ((,class (:foreground ,tokyo-green))))
         `(vc-conflict-state ((,class (:foreground ,tokyo-red :weight bold))))
         `(vc-edited-state ((,class (:foreground ,tokyo-yellow))))
         `(vc-locally-added-state ((,class (:foreground ,tokyo-git-add))))
         `(vc-locked-state ((,class (:foreground ,tokyo-orange :weight bold))))
         `(vc-missing-state ((,class (:foreground ,tokyo-red-dark))))
         `(vc-needs-update-state ((,class (:foreground ,tokyo-orange))))
         `(vc-removed-state ((,class (:foreground ,tokyo-red))))
         `(vc-up-to-date-state ((,class (:foreground ,tokyo-green))))

;;;;; diff-hl
         `(diff-hl-change ((,class (:foreground ,tokyo-git-change :background ,tokyo-blue7))))
         `(diff-hl-delete ((,class (:foreground ,tokyo-git-delete :background ,tokyo-diff-del-bg))))
         `(diff-hl-insert ((,class (:foreground ,tokyo-git-add :background ,tokyo-diff-add-bg))))

;;;;; smerge
         `(smerge-base ((,class (:background ,tokyo-diff-chg-bg :extend t))))
         `(smerge-markers ((,class (:foreground ,tokyo-comment :background ,tokyo-bg-dark :extend t))))
         `(smerge-upper ((,class (:background ,tokyo-diff-del-bg :extend t))))
         `(smerge-lower ((,class (:background ,tokyo-diff-add-bg :extend t))))
         `(smerge-refined-added ((,class (:foreground ,tokyo-green :background ,tokyo-diff-add-bg :weight bold :extend t))))
         `(smerge-refined-removed ((,class (:foreground ,tokyo-red :background ,tokyo-diff-del-bg :weight bold :extend t))))
         `(smerge-refined-changed ((,class (:foreground ,tokyo-blue :background ,tokyo-diff-chg-bg :weight bold :extend t))))

;;;; Third-party packages
;;;;; evil
         `(evil-ex-commands ((,class (:foreground ,tokyo-fg-dark :underline t :slant italic))))
         `(evil-ex-info ((,class (:foreground ,tokyo-red :slant italic))))
         `(evil-ex-search ((,class (:foreground ,tokyo-bg :background ,tokyo-magenta-hot :weight bold))))
         `(evil-ex-lazy-highlight ((,class (:foreground ,tokyo-fg :background ,tokyo-blue-dark))))
         `(evil-ex-substitute-matches ((,class (:foreground ,tokyo-fg :background ,tokyo-blue-dark))))
         `(evil-ex-substitute-replacement ((,class (:foreground ,tokyo-orange :underline t :weight bold))))

;;;;; avy
         `(avy-lead-face ((,class (:foreground ,tokyo-bg :background ,tokyo-magenta-hot :weight bold))))
         `(avy-lead-face-0 ((,class (:foreground ,tokyo-bg :background ,tokyo-blue :weight bold))))
         `(avy-lead-face-1 ((,class (:foreground ,tokyo-bg :background ,tokyo-dark5 :weight bold))))
         `(avy-lead-face-2 ((,class (:foreground ,tokyo-bg :background ,tokyo-teal :weight bold))))
         `(avy-background-face ((,class (:foreground ,tokyo-comment))))
         `(avy-goto-char-timer-face ((,class (:foreground ,tokyo-bg :background ,tokyo-magenta-hot))))

;;;;; company
         `(company-tooltip ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-dark))))
         `(company-tooltip-selection ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-highlight))))
         `(company-tooltip-deprecated ((,class (:foreground ,tokyo-comment :strike-through t))))
         `(company-tooltip-search ((,class (:foreground ,tokyo-magenta-hot :weight bold))))
         `(company-tooltip-search-selection ((,class (:foreground ,tokyo-magenta-hot :background ,tokyo-bg-highlight :weight bold))))
         `(company-tooltip-mouse ((,class (:background ,tokyo-bg-highlight))))
         `(company-tooltip-common ((,class (:foreground ,tokyo-blue :weight bold))))
         `(company-tooltip-common-selection ((,class (:foreground ,tokyo-blue :background ,tokyo-bg-highlight :weight bold))))
         `(company-tooltip-annotation ((,class (:foreground ,tokyo-comment))))
         `(company-tooltip-annotation-selection ((,class (:foreground ,tokyo-fg-dark))))
         `(company-tooltip-quick-access ((,class (:foreground ,tokyo-dark5))))
         `(company-tooltip-quick-access-selection ((,class (:foreground ,tokyo-fg-dark))))
         `(company-tooltip-scrollbar-thumb ((,class (:background ,tokyo-selection))))
         `(company-tooltip-scrollbar-track ((,class (:background ,tokyo-bg-highlight))))
         `(company-preview ((,class (:foreground ,tokyo-comment))))
         `(company-preview-common ((,class (:foreground ,tokyo-blue))))
         `(company-preview-search ((,class (:foreground ,tokyo-magenta-hot))))
         `(company-echo ((,class (:foreground ,tokyo-fg))))
         `(company-echo-common ((,class (:foreground ,tokyo-blue :weight bold))))

;;;;; consult
         `(consult-preview-line ((,class (:background ,tokyo-bg-highlight))))
         `(consult-highlight-match ((,class (:foreground ,tokyo-magenta-hot :weight bold))))
         `(consult-highlight-mark ((,class (:foreground ,tokyo-orange :weight bold))))
         `(consult-preview-match ((,class (:inherit consult-highlight-match))))
         `(consult-preview-insertion ((,class (:foreground ,tokyo-green))))
         `(consult-narrow-indicator ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(consult-async-running ((,class (:foreground ,tokyo-orange))))
         `(consult-async-finished ((,class (:foreground ,tokyo-green))))
         `(consult-async-failed ((,class (:foreground ,tokyo-red-dark))))
         `(consult-async-split ((,class (:foreground ,tokyo-magenta))))
         `(consult-key ((,class (:foreground ,tokyo-blue))))
         `(consult-line-number ((,class (:foreground ,tokyo-dark5))))
         `(consult-file ((,class (:foreground ,tokyo-blue))))
         `(consult-grep-context ((,class (:foreground ,tokyo-fg-dark))))
         `(consult-bookmark ((,class (:foreground ,tokyo-yellow))))
         `(consult-buffer ((,class (:foreground ,tokyo-fg))))
         `(consult-line-number-prefix ((,class (:foreground ,tokyo-dark5))))
         `(consult-line-number-wrapped ((,class (:foreground ,tokyo-orange))))

;;;;; corfu
         `(corfu-default ((,class (:background ,tokyo-bg-dark))))
         `(corfu-current ((,class (:background ,tokyo-bg-highlight))))
         `(corfu-bar ((,class (:background ,tokyo-selection))))
         `(corfu-border ((,class (:background ,tokyo-fg-gutter))))
         `(corfu-annotations ((,class (:foreground ,tokyo-comment))))
         `(corfu-deprecated ((,class (:foreground ,tokyo-comment :strike-through t))))

;;;;; embark
         `(embark-keybinding ((,class (:foreground ,tokyo-blue :weight bold))))
         `(embark-keybinding-repeat ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(embark-target ((,class (:foreground ,tokyo-orange :weight bold))))
         `(embark-verbose-indicator-documentation ((,class (:foreground ,tokyo-comment :slant italic))))
         `(embark-verbose-indicator-title ((,class (:foreground ,tokyo-fg :weight bold))))
         `(embark-verbose-indicator-shadowed ((,class (:foreground ,tokyo-dark5))))
         `(embark-collect-candidate ((,class (:foreground ,tokyo-fg))))
         `(embark-collect-group-title ((,class (:foreground ,tokyo-magenta :weight bold :slant italic))))
         `(embark-collect-group-separator ((,class (:foreground ,tokyo-comment :strike-through t))))
         `(embark-collect-annotation ((,class (:foreground ,tokyo-comment))))
         `(embark-selected ((,class (:background ,tokyo-bg-highlight :weight bold))))

;;;;; flycheck
         `(flycheck-error ((,class (:underline (:style wave :color ,tokyo-red-dark)))))
         `(flycheck-warning ((,class (:underline (:style wave :color ,tokyo-yellow)))))
         `(flycheck-info ((,class (:underline (:style wave :color ,tokyo-teal)))))
         `(flycheck-fringe-error ((,class (:foreground ,tokyo-red-dark))))
         `(flycheck-fringe-warning ((,class (:foreground ,tokyo-yellow))))
         `(flycheck-fringe-info ((,class (:foreground ,tokyo-teal))))
         `(flycheck-error-list-error ((,class (:foreground ,tokyo-red-dark))))
         `(flycheck-error-list-warning ((,class (:foreground ,tokyo-yellow))))
         `(flycheck-error-list-info ((,class (:foreground ,tokyo-teal))))
         `(flycheck-error-list-filename ((,class (:foreground ,tokyo-blue))))
         `(flycheck-error-list-line-number ((,class (:foreground ,tokyo-dark5))))
         `(flycheck-error-list-column-number ((,class (:foreground ,tokyo-dark5))))
         `(flycheck-error-list-id ((,class (:foreground ,tokyo-comment))))
         `(flycheck-error-list-checker-name ((,class (:foreground ,tokyo-comment))))
         `(flycheck-error-list-highlight ((,class (:background ,tokyo-bg-highlight))))

;;;;; git-commit
         `(git-commit-summary ((,class (:foreground ,tokyo-fg :weight bold))))
         `(git-commit-overlong-summary ((,class (:foreground ,tokyo-red :weight bold))))
         `(git-commit-nonempty-second-line ((,class (:foreground ,tokyo-yellow))))
         `(git-commit-keyword ((,class (:foreground ,tokyo-magenta :slant italic))))
         `(git-commit-trailer-token ((,class (:foreground ,tokyo-magenta))))
         `(git-commit-trailer-value ((,class (:foreground ,tokyo-fg-dark))))
         `(git-commit-comment-branch-local ((,class (:foreground ,tokyo-blue :weight bold))))
         `(git-commit-comment-branch-remote ((,class (:foreground ,tokyo-teal :weight bold))))
         `(git-commit-comment-detached ((,class (:foreground ,tokyo-orange :weight bold))))
         `(git-commit-comment-heading ((,class (:foreground ,tokyo-fg :weight bold))))
         `(git-commit-comment-file ((,class (:foreground ,tokyo-fg-dark))))
         `(git-commit-comment-action ((,class (:foreground ,tokyo-comment))))

;;;;; git-rebase
         `(git-rebase-hash ((,class (:foreground ,tokyo-dark5))))
         `(git-rebase-label ((,class (:foreground ,tokyo-magenta))))
         `(git-rebase-description ((,class (:foreground ,tokyo-fg-dark))))
         `(git-rebase-action ((,class (:foreground ,tokyo-blue :weight bold))))
         `(git-rebase-killed-action ((,class (:foreground ,tokyo-comment :strike-through t))))
         `(git-rebase-comment-hash ((,class (:foreground ,tokyo-dark5))))
         `(git-rebase-comment-heading ((,class (:foreground ,tokyo-fg :weight bold))))

;;;;; helpful
         `(helpful-heading ((,class (:foreground ,tokyo-blue :weight bold :height 1.2))))

;;;;; ivy
         `(ivy-current-match ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-highlight :weight bold :extend t))))
         `(ivy-minibuffer-match-highlight ((,class (:foreground ,tokyo-orange))))
         `(ivy-minibuffer-match-face-1 ((,class (:foreground ,tokyo-dark5))))
         `(ivy-minibuffer-match-face-2 ((,class (:foreground ,tokyo-blue :weight bold))))
         `(ivy-minibuffer-match-face-3 ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(ivy-minibuffer-match-face-4 ((,class (:foreground ,tokyo-teal :weight bold))))
         `(ivy-confirm-face ((,class (:foreground ,tokyo-green :weight bold))))
         `(ivy-match-required-face ((,class (:foreground ,tokyo-red :weight bold))))
         `(ivy-subdir ((,class (:foreground ,tokyo-blue :weight bold))))
         `(ivy-remote ((,class (:foreground ,tokyo-magenta))))
         `(ivy-virtual ((,class (:foreground ,tokyo-comment :slant italic))))
         `(ivy-action ((,class (:foreground ,tokyo-orange :weight bold))))
         `(ivy-highlight-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(ivy-modified-buffer ((,class (:foreground ,tokyo-yellow))))
         `(ivy-modified-outside-buffer ((,class (:foreground ,tokyo-red))))
         `(ivy-separator ((,class (:foreground ,tokyo-comment))))
         `(ivy-grep-info ((,class (:foreground ,tokyo-teal))))
         `(ivy-grep-line-number ((,class (:foreground ,tokyo-dark5))))
         `(ivy-completions-annotations ((,class (:foreground ,tokyo-comment))))
         `(ivy-yanked-word ((,class (:foreground ,tokyo-magenta-hot :weight bold))))
         `(ivy-cursor ((,class (:foreground ,tokyo-bg :background ,tokyo-fg))))
         `(ivy-prompt-match ((,class (:inherit ivy-current-match))))
         `(ivy-org ((,class (:foreground ,tokyo-green))))

;;;;; magit
         `(magit-section-heading ((,class (:foreground ,tokyo-blue :weight bold))))
         `(magit-section-heading-selection ((,class (:foreground ,tokyo-orange :weight bold))))
         `(magit-section-secondary-heading ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(magit-section-highlight ((,class (:background ,tokyo-bg-highlight))))
         `(magit-section-child-count ((,class (:foreground ,tokyo-dark5))))
         `(magit-header-line ((,class (:foreground ,tokyo-blue :weight bold :background ,tokyo-bg-dark))))
         `(magit-header-line-key ((,class (:foreground ,tokyo-blue))))
         `(magit-dimmed ((,class (:foreground ,tokyo-comment))))
         `(magit-hash ((,class (:foreground ,tokyo-dark5))))
         `(magit-tag ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(magit-keyword ((,class (:foreground ,tokyo-magenta))))
         `(magit-keyword-squash ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(magit-filename ((,class (:foreground ,tokyo-fg-dark))))
         `(magit-head ((,class (:foreground ,tokyo-blue :weight bold))))
         `(magit-branch-local ((,class (:foreground ,tokyo-blue))))
         `(magit-branch-current ((,class (:foreground ,tokyo-blue :weight bold :box (:line-width -1 :color ,tokyo-blue-dark)))))
         `(magit-branch-remote ((,class (:foreground ,tokyo-teal))))
         `(magit-branch-remote-head ((,class (:foreground ,tokyo-teal :weight bold :box (:line-width -1 :color ,tokyo-teal-dark)))))
         `(magit-branch-upstream ((,class (:slant italic))))
         `(magit-branch-warning ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(magit-refname ((,class (:foreground ,tokyo-dark5))))
         `(magit-refname-stash ((,class (:foreground ,tokyo-dark5))))
         `(magit-refname-wip ((,class (:foreground ,tokyo-dark5 :slant italic))))
         `(magit-refname-pullreq ((,class (:foreground ,tokyo-dark5))))
         `(magit-signature-good ((,class (:foreground ,tokyo-green))))
         `(magit-signature-bad ((,class (:foreground ,tokyo-red :weight bold))))
         `(magit-signature-untrusted ((,class (:foreground ,tokyo-yellow))))
         `(magit-signature-expired ((,class (:foreground ,tokyo-orange))))
         `(magit-signature-expired-key ((,class (:foreground ,tokyo-orange))))
         `(magit-signature-revoked ((,class (:foreground ,tokyo-magenta))))
         `(magit-signature-error ((,class (:foreground ,tokyo-red-dark))))
         `(magit-cherry-unmatched ((,class (:foreground ,tokyo-cyan))))
         `(magit-cherry-equivalent ((,class (:foreground ,tokyo-magenta))))
;;;;;; magit-diff
         `(magit-diff-file-heading ((,class (:foreground ,tokyo-fg :weight bold :extend t))))
         `(magit-diff-file-heading-highlight ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-highlight :weight bold :extend t))))
         `(magit-diff-file-heading-selection ((,class (:foreground ,tokyo-orange :background ,tokyo-bg-highlight :weight bold :extend t))))
         `(magit-diff-hunk-heading ((,class (:foreground ,tokyo-fg-dark :background ,tokyo-bg-dark :extend t))))
         `(magit-diff-hunk-heading-highlight ((,class (:foreground ,tokyo-fg :background ,tokyo-terminal-blk :extend t))))
         `(magit-diff-hunk-heading-selection ((,class (:foreground ,tokyo-orange :background ,tokyo-terminal-blk :extend t))))
         `(magit-diff-conflict-heading ((,class (:foreground ,tokyo-yellow :background ,tokyo-bg-dark :extend t))))
         `(magit-diff-conflict-heading-highlight ((,class (:foreground ,tokyo-yellow :background ,tokyo-terminal-blk :extend t))))
         `(magit-diff-revision-summary ((,class (:foreground ,tokyo-fg :weight bold))))
         `(magit-diff-revision-summary-highlight ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-highlight :weight bold))))
         `(magit-diff-lines-heading ((,class (:foreground ,tokyo-bg :background ,tokyo-blue :extend t))))
         `(magit-diff-context ((,class (:foreground ,tokyo-comment :extend t))))
         `(magit-diff-context-highlight ((,class (:foreground ,tokyo-fg-dark :background ,tokyo-bg-line :extend t))))
         `(magit-diff-added ((,class (:foreground ,tokyo-git-add :background ,tokyo-diff-add-bg :extend t))))
         `(magit-diff-added-highlight ((,class (:foreground ,tokyo-git-add :background ,tokyo-diff-add-bg :weight bold :extend t))))
         `(magit-diff-removed ((,class (:foreground ,tokyo-git-delete :background ,tokyo-diff-del-bg :extend t))))
         `(magit-diff-removed-highlight ((,class (:foreground ,tokyo-git-delete :background ,tokyo-diff-del-bg :weight bold :extend t))))
         `(magit-diff-our ((,class (:inherit magit-diff-removed))))
         `(magit-diff-our-highlight ((,class (:inherit magit-diff-removed-highlight))))
         `(magit-diff-base ((,class (:foreground ,tokyo-yellow :background ,tokyo-diff-chg-bg :extend t))))
         `(magit-diff-base-highlight ((,class (:foreground ,tokyo-yellow :background ,tokyo-diff-chg-bg :weight bold :extend t))))
         `(magit-diff-their ((,class (:inherit magit-diff-added))))
         `(magit-diff-their-highlight ((,class (:inherit magit-diff-added-highlight))))
         `(magit-diffstat-added ((,class (:foreground ,tokyo-git-add))))
         `(magit-diffstat-removed ((,class (:foreground ,tokyo-git-delete))))
         `(magit-diff-whitespace-warning ((,class (:background ,tokyo-red))))
;;;;;; magit-log
         `(magit-log-graph ((,class (:foreground ,tokyo-dark5))))
         `(magit-log-author ((,class (:foreground ,tokyo-orange))))
         `(magit-log-date ((,class (:foreground ,tokyo-cyan))))
         `(magit-header-line-log-select ((,class (:foreground ,tokyo-fg :weight bold))))
;;;;;; magit-process
         `(magit-process-ok ((,class (:foreground ,tokyo-green :weight bold))))
         `(magit-process-ng ((,class (:foreground ,tokyo-red :weight bold))))
         `(magit-mode-line-process ((,class (:foreground ,tokyo-blue))))
         `(magit-mode-line-process-error ((,class (:foreground ,tokyo-red))))
;;;;;; magit-sequence
         `(magit-sequence-pick ((,class (:foreground ,tokyo-fg-dark))))
         `(magit-sequence-stop ((,class (:foreground ,tokyo-green))))
         `(magit-sequence-part ((,class (:foreground ,tokyo-yellow))))
         `(magit-sequence-head ((,class (:foreground ,tokyo-blue))))
         `(magit-sequence-drop ((,class (:foreground ,tokyo-red))))
         `(magit-sequence-done ((,class (:foreground ,tokyo-comment))))
         `(magit-sequence-onto ((,class (:foreground ,tokyo-comment))))
         `(magit-sequence-exec ((,class (:foreground ,tokyo-comment))))
;;;;;; magit-bisect
         `(magit-bisect-good ((,class (:foreground ,tokyo-green))))
         `(magit-bisect-skip ((,class (:foreground ,tokyo-yellow))))
         `(magit-bisect-bad ((,class (:foreground ,tokyo-red))))
;;;;;; magit-blame
         `(magit-blame-highlight ((,class (:background ,tokyo-bg-highlight :extend t))))
         `(magit-blame-margin ((,class (:foreground ,tokyo-fg-dark :background ,tokyo-bg-dark :extend t))))
         `(magit-blame-dimmed ((,class (:foreground ,tokyo-comment))))
         `(magit-blame-heading ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-dark :extend t))))
         `(magit-blame-summary ((,class (:foreground ,tokyo-fg-dark))))
         `(magit-blame-hash ((,class (:foreground ,tokyo-dark5))))
         `(magit-blame-name ((,class (:foreground ,tokyo-orange))))
         `(magit-blame-date ((,class (:foreground ,tokyo-cyan))))
;;;;;; magit-reflog
         `(magit-reflog-commit ((,class (:foreground ,tokyo-green))))
         `(magit-reflog-amend ((,class (:foreground ,tokyo-yellow))))
         `(magit-reflog-merge ((,class (:foreground ,tokyo-teal))))
         `(magit-reflog-checkout ((,class (:foreground ,tokyo-blue))))
         `(magit-reflog-reset ((,class (:foreground ,tokyo-red))))
         `(magit-reflog-rebase ((,class (:foreground ,tokyo-magenta))))
         `(magit-reflog-cherry-pick ((,class (:foreground ,tokyo-green))))
         `(magit-reflog-remote ((,class (:foreground ,tokyo-cyan))))
         `(magit-reflog-other ((,class (:foreground ,tokyo-dark5))))

;;;;; marginalia
         `(marginalia-key ((,class (:foreground ,tokyo-blue))))
         `(marginalia-type ((,class (:foreground ,tokyo-cyan-bright))))
         `(marginalia-char ((,class (:foreground ,tokyo-orange))))
         `(marginalia-lighter ((,class (:foreground ,tokyo-dark5))))
         `(marginalia-on ((,class (:foreground ,tokyo-green))))
         `(marginalia-off ((,class (:foreground ,tokyo-comment))))
         `(marginalia-documentation ((,class (:foreground ,tokyo-comment :slant italic))))
         `(marginalia-value ((,class (:foreground ,tokyo-fg-dark))))
         `(marginalia-null ((,class (:foreground ,tokyo-comment))))
         `(marginalia-true ((,class (:foreground ,tokyo-green))))
         `(marginalia-function ((,class (:foreground ,tokyo-blue))))
         `(marginalia-symbol ((,class (:foreground ,tokyo-magenta))))
         `(marginalia-list ((,class (:foreground ,tokyo-teal))))
         `(marginalia-mode ((,class (:foreground ,tokyo-cyan))))
         `(marginalia-date ((,class (:foreground ,tokyo-cyan))))
         `(marginalia-version ((,class (:foreground ,tokyo-teal))))
         `(marginalia-archive ((,class (:foreground ,tokyo-magenta))))
         `(marginalia-installed ((,class (:foreground ,tokyo-green))))
         `(marginalia-size ((,class (:foreground ,tokyo-dark5))))
         `(marginalia-number ((,class (:foreground ,tokyo-orange))))
         `(marginalia-string ((,class (:foreground ,tokyo-green))))
         `(marginalia-modified ((,class (:foreground ,tokyo-yellow))))
         `(marginalia-file-name ((,class (:foreground ,tokyo-fg-dark))))
         `(marginalia-file-owner ((,class (:foreground ,tokyo-comment))))
         `(marginalia-file-priv-no ((,class (:foreground ,tokyo-comment))))
         `(marginalia-file-priv-dir ((,class (:foreground ,tokyo-blue :weight bold))))
         `(marginalia-file-priv-link ((,class (:foreground ,tokyo-cyan))))
         `(marginalia-file-priv-read ((,class (:foreground ,tokyo-yellow))))
         `(marginalia-file-priv-write ((,class (:foreground ,tokyo-red))))
         `(marginalia-file-priv-exec ((,class (:foreground ,tokyo-green))))
         `(marginalia-file-priv-other ((,class (:foreground ,tokyo-magenta))))
         `(marginalia-file-priv-rare ((,class (:foreground ,tokyo-orange))))

;;;;; markdown-mode
         `(markdown-header-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(markdown-header-face-1 ((,class (:foreground ,tokyo-heading1 :weight bold :height ,h1))))
         `(markdown-header-face-2 ((,class (:foreground ,tokyo-heading2 :weight bold :height ,h2))))
         `(markdown-header-face-3 ((,class (:foreground ,tokyo-heading3 :weight bold :height ,h3))))
         `(markdown-header-face-4 ((,class (:foreground ,tokyo-heading4 :weight bold))))
         `(markdown-header-face-5 ((,class (:foreground ,tokyo-heading5 :weight bold))))
         `(markdown-header-face-6 ((,class (:foreground ,tokyo-heading6 :weight bold))))
         `(markdown-header-delimiter-face ((,class (:foreground ,tokyo-comment))))
         `(markdown-header-rule-face ((,class (:foreground ,tokyo-comment))))
         `(markdown-bold-face ((,class (:foreground ,tokyo-orange :weight bold))))
         `(markdown-italic-face ((,class (:foreground ,tokyo-fg :slant italic))))
         `(markdown-strike-through-face ((,class (:foreground ,tokyo-comment :strike-through t))))
         `(markdown-markup-face ((,class (:foreground ,tokyo-dark5))))
         `(markdown-list-face ((,class (:foreground ,tokyo-magenta))))
         `(markdown-blockquote-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(markdown-code-face ((,class (:foreground ,tokyo-teal :background ,tokyo-bg-dark :extend t))))
         `(markdown-inline-code-face ((,class (:foreground ,tokyo-teal :background ,tokyo-bg-dark))))
         `(markdown-pre-face ((,class (:foreground ,tokyo-teal :background ,tokyo-bg-dark))))
         `(markdown-table-face ((,class (:foreground ,tokyo-fg-muted))))
         `(markdown-language-keyword-face ((,class (:foreground ,tokyo-magenta))))
         `(markdown-language-info-face ((,class (:foreground ,tokyo-dark5))))
         `(markdown-link-face ((,class (:foreground ,tokyo-teal))))
         `(markdown-missing-link-face ((,class (:foreground ,tokyo-red :weight bold))))
         `(markdown-reference-face ((,class (:foreground ,tokyo-dark5))))
         `(markdown-footnote-marker-face ((,class (:foreground ,tokyo-dark5))))
         `(markdown-footnote-text-face ((,class (:foreground ,tokyo-fg-dark))))
         `(markdown-url-face ((,class (:foreground ,tokyo-cyan :underline t))))
         `(markdown-plain-url-face ((,class (:foreground ,tokyo-cyan :underline t))))
         `(markdown-link-title-face ((,class (:foreground ,tokyo-green :slant italic))))
         `(markdown-line-break-face ((,class (:background ,tokyo-bg-highlight))))
         `(markdown-comment-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(markdown-math-face ((,class (:foreground ,tokyo-orange))))
         `(markdown-metadata-key-face ((,class (:foreground ,tokyo-magenta))))
         `(markdown-metadata-value-face ((,class (:foreground ,tokyo-fg-dark))))
         `(markdown-gfm-checkbox-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(markdown-highlight-face ((,class (:foreground ,tokyo-bg :background ,tokyo-yellow))))
         `(markdown-hr-face ((,class (:foreground ,tokyo-comment))))
         `(markdown-html-tag-name-face ((,class (:foreground ,tokyo-red))))
         `(markdown-html-tag-delimiter-face ((,class (:foreground ,tokyo-dark5))))
         `(markdown-html-attr-name-face ((,class (:foreground ,tokyo-magenta))))
         `(markdown-html-attr-value-face ((,class (:foreground ,tokyo-green))))
         `(markdown-html-entity-face ((,class (:foreground ,tokyo-orange))))

;;;;; orderless
         `(orderless-match-face-0 ((,class (:foreground ,tokyo-blue :weight bold))))
         `(orderless-match-face-1 ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(orderless-match-face-2 ((,class (:foreground ,tokyo-teal :weight bold))))
         `(orderless-match-face-3 ((,class (:foreground ,tokyo-orange :weight bold))))

;;;;; rainbow-delimiters
         `(rainbow-delimiters-base-face ((,class (:foreground ,tokyo-fg-dark))))
         `(rainbow-delimiters-base-error-face ((,class (:foreground ,tokyo-red :weight bold))))
         `(rainbow-delimiters-unmatched-face ((,class (:foreground ,tokyo-red :weight bold))))
         `(rainbow-delimiters-mismatched-face ((,class (:foreground ,tokyo-red :weight bold))))
         `(rainbow-delimiters-depth-1-face ((,class (:foreground ,tokyo-blue))))
         `(rainbow-delimiters-depth-2-face ((,class (:foreground ,tokyo-magenta))))
         `(rainbow-delimiters-depth-3-face ((,class (:foreground ,tokyo-teal))))
         `(rainbow-delimiters-depth-4-face ((,class (:foreground ,tokyo-orange))))
         `(rainbow-delimiters-depth-5-face ((,class (:foreground ,tokyo-cyan))))
         `(rainbow-delimiters-depth-6-face ((,class (:foreground ,tokyo-yellow))))
         `(rainbow-delimiters-depth-7-face ((,class (:foreground ,tokyo-green))))
         `(rainbow-delimiters-depth-8-face ((,class (:foreground ,tokyo-magenta-dark))))
         `(rainbow-delimiters-depth-9-face ((,class (:foreground ,tokyo-cyan-bright))))

;;;;; swiper
         `(swiper-line-face ((,class (:background ,tokyo-bg-highlight :extend t))))
         `(swiper-match-face-1 ((,class (:foreground ,tokyo-dark5))))
         `(swiper-match-face-2 ((,class (:foreground ,tokyo-bg :background ,tokyo-blue :weight bold))))
         `(swiper-match-face-3 ((,class (:foreground ,tokyo-bg :background ,tokyo-magenta :weight bold))))
         `(swiper-match-face-4 ((,class (:foreground ,tokyo-bg :background ,tokyo-teal :weight bold))))
         `(swiper-background-match-face-1 ((,class (:foreground ,tokyo-dark5))))
         `(swiper-background-match-face-2 ((,class (:foreground ,tokyo-blue :weight bold))))
         `(swiper-background-match-face-3 ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(swiper-background-match-face-4 ((,class (:foreground ,tokyo-teal :weight bold))))

;;;;; transient
         `(transient-heading ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(transient-argument ((,class (:foreground ,tokyo-orange :weight bold))))
         `(transient-inactive-argument ((,class (:foreground ,tokyo-comment))))
         `(transient-inapt-argument ((,class (:foreground ,tokyo-comment :slant italic))))
         `(transient-value ((,class (:foreground ,tokyo-teal :weight bold))))
         `(transient-inactive-value ((,class (:foreground ,tokyo-comment))))
         `(transient-delimiter ((,class (:foreground ,tokyo-dark5))))
         `(transient-unreachable ((,class (:foreground ,tokyo-comment))))
         `(transient-inapt-suffix ((,class (:foreground ,tokyo-comment :slant italic))))
         `(transient-active-infix ((,class (:background ,tokyo-bg-highlight))))
         `(transient-enabled-suffix ((,class (:foreground ,tokyo-green :background ,tokyo-diff-add-bg :weight bold))))
         `(transient-disabled-suffix ((,class (:foreground ,tokyo-red :background ,tokyo-diff-del-bg :weight bold))))
         `(transient-higher-level ((,class (:underline t))))
         `(transient-key ((,class (:foreground ,tokyo-blue))))
         `(transient-key-stay ((,class (:foreground ,tokyo-teal :weight bold))))
         `(transient-key-noop ((,class (:foreground ,tokyo-comment))))
         `(transient-key-return ((,class (:foreground ,tokyo-magenta))))
         `(transient-key-recurse ((,class (:foreground ,tokyo-orange))))
         `(transient-key-stack ((,class (:foreground ,tokyo-blue :weight bold))))
         `(transient-key-exit ((,class (:foreground ,tokyo-red))))
         `(transient-unreachable-key ((,class (:foreground ,tokyo-comment))))
         `(transient-nonstandard-key ((,class (:foreground ,tokyo-yellow :underline t))))
         `(transient-mismatched-key ((,class (:foreground ,tokyo-yellow :underline t))))

;;;;; vertico
         `(vertico-current ((,class (:background ,tokyo-bg-highlight :extend t))))
         `(vertico-multiline ((,class (:foreground ,tokyo-comment))))
         `(vertico-group-title ((,class (:foreground ,tokyo-magenta :weight bold :slant italic))))
         `(vertico-group-separator ((,class (:foreground ,tokyo-comment :strike-through t))))

;;;;; which-key
         `(which-key-key-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(which-key-separator-face ((,class (:foreground ,tokyo-comment))))
         `(which-key-note-face ((,class (:foreground ,tokyo-comment))))
         `(which-key-command-description-face ((,class (:foreground ,tokyo-fg))))
         `(which-key-local-map-description-face ((,class (:foreground ,tokyo-teal))))
         `(which-key-highlighted-command-face ((,class (:foreground ,tokyo-orange :weight bold))))
         `(which-key-group-description-face ((,class (:foreground ,tokyo-magenta))))
         `(which-key-special-key-face ((,class (:foreground ,tokyo-red :weight bold))))
         `(which-key-docstring-face ((,class (:foreground ,tokyo-comment :slant italic))))

;;;;; ace-window
         `(aw-leading-char-face ((,class (:foreground ,tokyo-magenta-hot :weight bold :height 2.0))))
         `(aw-minibuffer-leading-char-face ((,class (:foreground ,tokyo-magenta-hot :weight bold))))
         `(aw-background-face ((,class (:foreground ,tokyo-comment))))
         `(aw-mode-line-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(aw-key-face ((,class (:foreground ,tokyo-magenta-hot :weight bold))))

;;;;; cider
         `(cider-result-overlay-face ((,class (:foreground ,tokyo-teal :background ,tokyo-bg-dark :box (:line-width -1 :color ,tokyo-teal-dark)))))
         `(cider-error-overlay-face ((,class (:foreground ,tokyo-red :background ,tokyo-bg-dark :box (:line-width -1 :color ,tokyo-red-dark)))))
         `(cider-fringe-good-face ((,class (:foreground ,tokyo-green))))
         `(cider-fragile-button-face ((,class (:foreground ,tokyo-yellow :box (:line-width -1 :color ,tokyo-yellow)))))
         `(cider-test-failure-face ((,class (:foreground ,tokyo-bg :background ,tokyo-red))))
         `(cider-test-error-face ((,class (:foreground ,tokyo-bg :background ,tokyo-orange))))
         `(cider-test-success-face ((,class (:foreground ,tokyo-bg :background ,tokyo-green))))
         `(cider-debug-code-overlay-face ((,class (:background ,tokyo-bg-highlight))))
         `(cider-debug-prompt-face ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(cider-enlightened-face ((,class (:foreground ,tokyo-yellow :background ,tokyo-bg-dark :box (:line-width -1 :color ,tokyo-yellow)))))
         `(cider-enlightened-local-face ((,class (:foreground ,tokyo-yellow :weight bold))))

;;;;; doom-modeline
         `(doom-modeline ((,class (:inherit mode-line))))
         `(doom-modeline-emphasis ((,class (:foreground ,tokyo-blue :weight bold))))
         `(doom-modeline-highlight ((,class (:foreground ,tokyo-magenta))))
         `(doom-modeline-buffer-path ((,class (:foreground ,tokyo-comment))))
         `(doom-modeline-buffer-file ((,class (:foreground ,tokyo-fg :weight bold))))
         `(doom-modeline-buffer-modified ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(doom-modeline-buffer-major-mode ((,class (:foreground ,tokyo-blue :weight bold))))
         `(doom-modeline-buffer-minor-mode ((,class (:foreground ,tokyo-dark5))))
         `(doom-modeline-project-parent-dir ((,class (:foreground ,tokyo-comment))))
         `(doom-modeline-project-dir ((,class (:foreground ,tokyo-blue :weight bold))))
         `(doom-modeline-project-root-dir ((,class (:foreground ,tokyo-fg))))
         `(doom-modeline-panel ((,class (:foreground ,tokyo-bg :background ,tokyo-blue))))
         `(doom-modeline-host ((,class (:foreground ,tokyo-magenta :slant italic))))
         `(doom-modeline-input-method ((,class (:foreground ,tokyo-orange :weight bold))))
         `(doom-modeline-input-method-alt ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(doom-modeline-debug ((,class (:foreground ,tokyo-orange))))
         `(doom-modeline-info ((,class (:foreground ,tokyo-teal))))
         `(doom-modeline-warning ((,class (:foreground ,tokyo-yellow))))
         `(doom-modeline-urgent ((,class (:foreground ,tokyo-red))))
         `(doom-modeline-notification ((,class (:foreground ,tokyo-orange))))
         `(doom-modeline-unread-number ((,class (:foreground ,tokyo-fg :weight bold))))
         `(doom-modeline-bar ((,class (:background ,tokyo-blue))))
         `(doom-modeline-bar-inactive ((,class (:background ,tokyo-bg-dark))))
         `(doom-modeline-debug-visual ((,class (:foreground ,tokyo-orange :background ,tokyo-bg-dark))))
         `(doom-modeline-evil-emacs-state ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(doom-modeline-evil-insert-state ((,class (:foreground ,tokyo-green :weight bold))))
         `(doom-modeline-evil-motion-state ((,class (:foreground ,tokyo-blue :weight bold))))
         `(doom-modeline-evil-normal-state ((,class (:foreground ,tokyo-teal :weight bold))))
         `(doom-modeline-evil-operator-state ((,class (:foreground ,tokyo-magenta-dark :weight bold))))
         `(doom-modeline-evil-visual-state ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(doom-modeline-evil-replace-state ((,class (:foreground ,tokyo-red :weight bold))))
         `(doom-modeline-overwrite ((,class (:foreground ,tokyo-red :weight bold))))
         `(doom-modeline-project-name ((,class (:foreground ,tokyo-blue :weight bold))))
         `(doom-modeline-workspace-name ((,class (:foreground ,tokyo-magenta))))
         `(doom-modeline-persp-name ((,class (:foreground ,tokyo-cyan :weight bold))))
         `(doom-modeline-persp-buffer-not-in-persp ((,class (:foreground ,tokyo-comment :slant italic))))
         `(doom-modeline-repl-success ((,class (:foreground ,tokyo-green :weight bold))))
         `(doom-modeline-repl-warning ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(doom-modeline-vcs-default ((,class (:foreground ,tokyo-fg-dark))))
         `(doom-modeline-lsp-success ((,class (:foreground ,tokyo-green :weight bold))))
         `(doom-modeline-lsp-warning ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(doom-modeline-lsp-error ((,class (:foreground ,tokyo-red :weight bold))))
         `(doom-modeline-lsp-running ((,class (:foreground ,tokyo-orange))))
         `(doom-modeline-battery-charging ((,class (:foreground ,tokyo-green))))
         `(doom-modeline-battery-full ((,class (:foreground ,tokyo-green))))
         `(doom-modeline-battery-normal ((,class (:foreground ,tokyo-fg-dark))))
         `(doom-modeline-battery-warning ((,class (:foreground ,tokyo-yellow :weight bold))))
         `(doom-modeline-battery-critical ((,class (:foreground ,tokyo-red :weight bold))))
         `(doom-modeline-battery-error ((,class (:foreground ,tokyo-red-dark :weight bold))))
         `(doom-modeline-time ((,class (:foreground ,tokyo-dark5))))
         `(doom-modeline-compilation ((,class (:foreground ,tokyo-orange :weight bold))))

;;;;; elfeed
         `(elfeed-search-date-face ((,class (:foreground ,tokyo-cyan))))
         `(elfeed-search-title-face ((,class (:foreground ,tokyo-comment))))
         `(elfeed-search-unread-title-face ((,class (:foreground ,tokyo-fg :weight bold))))
         `(elfeed-search-feed-face ((,class (:foreground ,tokyo-blue))))
         `(elfeed-search-tag-face ((,class (:foreground ,tokyo-teal))))
         `(elfeed-search-last-update-face ((,class (:foreground ,tokyo-dark5))))
         `(elfeed-search-unread-count-face ((,class (:foreground ,tokyo-orange :weight bold))))
         `(elfeed-search-filter-face ((,class (:foreground ,tokyo-magenta :weight bold))))

;;;;; forge
         `(forge-dimmed ((,class (:foreground ,tokyo-comment))))
         `(forge-topic-header-line ((,class (:foreground ,tokyo-fg :weight bold))))
         `(forge-topic-slug-open ((,class (:foreground ,tokyo-green))))
         `(forge-topic-slug-realized ((,class (:foreground ,tokyo-dark5))))
         `(forge-topic-slug-expunged ((,class (:foreground ,tokyo-comment :strike-through t))))
         `(forge-topic-slug-saved ((,class (:foreground ,tokyo-blue))))
         `(forge-topic-slug-unread ((,class (:foreground ,tokyo-fg :weight bold))))
         `(forge-topic-unread ((,class (:foreground ,tokyo-fg :weight bold))))
         `(forge-topic-pending ((,class (:foreground ,tokyo-yellow))))
         `(forge-topic-done ((,class (:foreground ,tokyo-comment))))
         `(forge-discussion-open ((,class (:foreground ,tokyo-green))))
         `(forge-discussion-completed ((,class (:foreground ,tokyo-comment))))
         `(forge-discussion-expunged ((,class (:foreground ,tokyo-comment :strike-through t))))
         `(forge-issue-open ((,class (:foreground ,tokyo-green))))
         `(forge-issue-completed ((,class (:foreground ,tokyo-comment))))
         `(forge-issue-expunged ((,class (:foreground ,tokyo-comment :strike-through t))))
         `(forge-pullreq-open ((,class (:foreground ,tokyo-green))))
         `(forge-pullreq-merged ((,class (:foreground ,tokyo-magenta))))
         `(forge-pullreq-rejected ((,class (:foreground ,tokyo-red))))
         `(forge-pullreq-draft ((,class (:foreground ,tokyo-dark5 :slant italic))))
         `(forge-topic-label ((,class (:box (:line-width -1 :color ,tokyo-terminal-blk)))))
         `(forge-post-author ((,class (:foreground ,tokyo-orange :weight bold))))
         `(forge-post-date ((,class (:foreground ,tokyo-cyan))))
         `(forge-suffix-active ((,class (:foreground ,tokyo-green :weight bold))))
         `(forge-suffix-active-and-implied ((,class (:foreground ,tokyo-green))))
         `(forge-suffix-implied ((,class (:foreground ,tokyo-dark5))))

;;;;; hydra
         `(hydra-face-red ((,class (:foreground ,tokyo-red :weight bold))))
         `(hydra-face-blue ((,class (:foreground ,tokyo-blue :weight bold))))
         `(hydra-face-amaranth ((,class (:foreground ,tokyo-orange :weight bold))))
         `(hydra-face-pink ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(hydra-face-teal ((,class (:foreground ,tokyo-teal :weight bold))))

;;;;; lsp-mode
         `(lsp-face-highlight-textual ((,class (:background ,tokyo-bg-highlight))))
         `(lsp-face-highlight-read ((,class (:background ,tokyo-bg-highlight))))
         `(lsp-face-highlight-write ((,class (:background ,tokyo-bg-highlight :weight bold))))

;;;;; lsp-ui
         `(lsp-ui-doc-background ((,class (:background ,tokyo-bg-dark))))
         `(lsp-ui-doc-header ((,class (:foreground ,tokyo-fg :background ,tokyo-blue-dark :weight bold))))
         `(lsp-ui-doc-highlight-hover ((,class (:background ,tokyo-bg-highlight))))
         `(lsp-ui-doc-url ((,class (:foreground ,tokyo-teal :underline t))))
         `(lsp-ui-peek-peek ((,class (:background ,tokyo-bg-dark))))
         `(lsp-ui-peek-list ((,class (:background ,tokyo-bg-dark))))
         `(lsp-ui-peek-filename ((,class (:foreground ,tokyo-orange :weight bold))))
         `(lsp-ui-peek-line-number ((,class (:foreground ,tokyo-dark5))))
         `(lsp-ui-peek-highlight ((,class (:foreground ,tokyo-magenta-hot :weight bold :box (:line-width -1 :color ,tokyo-magenta-hot)))))
         `(lsp-ui-peek-header ((,class (:foreground ,tokyo-fg :background ,tokyo-blue-dark :weight bold))))
         `(lsp-ui-peek-footer ((,class (:foreground ,tokyo-fg :background ,tokyo-blue-dark))))
         `(lsp-ui-peek-selection ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-highlight :weight bold))))
         `(lsp-ui-sideline-symbol ((,class (:foreground ,tokyo-dark5 :box (:line-width -1 :color ,tokyo-dark5)))))
         `(lsp-ui-sideline-current-symbol ((,class (:foreground ,tokyo-fg :weight bold :box (:line-width -1 :color ,tokyo-fg)))))
         `(lsp-ui-sideline-code-action ((,class (:foreground ,tokyo-yellow))))
         `(lsp-ui-sideline-symbol-info ((,class (:foreground ,tokyo-comment :slant italic))))
         `(lsp-ui-sideline-global ((,class (:foreground ,tokyo-comment))))

;;;;; smartparens
         `(sp-show-pair-match-face ((,class (:foreground ,tokyo-cyan-pale :background ,tokyo-bracket :weight bold))))
         `(sp-show-pair-mismatch-face ((,class (:foreground ,tokyo-red :background ,tokyo-bg :weight bold :underline t))))
         `(sp-pair-overlay-face ((,class (:background ,tokyo-bg-highlight))))
         `(sp-show-pair-match-content-face ((,class (:background ,tokyo-bg-highlight))))

;;;;; treemacs
         `(treemacs-directory-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(treemacs-directory-collapsed-face ((,class (:foreground ,tokyo-blue))))
         `(treemacs-window-background-face ((,class (:background ,tokyo-bg-dark))))
         `(treemacs-hl-line-face ((,class (:background ,tokyo-bg-highlight :extend t))))
         `(treemacs-file-face ((,class (:foreground ,tokyo-fg))))
         `(treemacs-root-face ((,class (:foreground ,tokyo-blue :weight bold :height 1.1))))
         `(treemacs-root-unreadable-face ((,class (:foreground ,tokyo-red-dark :weight bold :height 1.1))))
         `(treemacs-root-remote-face ((,class (:foreground ,tokyo-magenta :weight bold :height 1.1))))
         `(treemacs-root-remote-unreadable-face ((,class (:foreground ,tokyo-red-dark :slant italic :height 1.1))))
         `(treemacs-root-remote-disconnected-face ((,class (:foreground ,tokyo-comment :weight bold :height 1.1))))
         `(treemacs-term-node-face ((,class (:foreground ,tokyo-fg-dark))))
         `(treemacs-git-unmodified-face ((,class (:foreground ,tokyo-fg))))
         `(treemacs-git-modified-face ((,class (:foreground ,tokyo-yellow))))
         `(treemacs-git-renamed-face ((,class (:foreground ,tokyo-git-add))))
         `(treemacs-git-ignored-face ((,class (:foreground ,tokyo-git-ignored))))
         `(treemacs-git-untracked-face ((,class (:foreground ,tokyo-orange))))
         `(treemacs-git-added-face ((,class (:foreground ,tokyo-git-add))))
         `(treemacs-git-conflict-face ((,class (:foreground ,tokyo-red :weight bold))))
         `(treemacs-tags-face ((,class (:foreground ,tokyo-teal))))
         `(treemacs-help-title-face ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(treemacs-help-column-face ((,class (:foreground ,tokyo-blue))))
         `(treemacs-on-failure-pulse-face ((,class (:background ,tokyo-diff-del-bg))))
         `(treemacs-on-success-pulse-face ((,class (:background ,tokyo-diff-add-bg))))
         `(treemacs-fringe-indicator-face ((,class (:foreground ,tokyo-blue))))
         `(treemacs-header-button-face ((,class (:foreground ,tokyo-fg :background ,tokyo-bg-highlight :box (:line-width -1 :color ,tokyo-terminal-blk)))))
         `(treemacs-marked-file-face ((,class (:foreground ,tokyo-magenta :weight bold))))
         `(treemacs-git-commit-diff-face ((,class (:foreground ,tokyo-dark5))))
         `(treemacs-async-loading-face ((,class (:foreground ,tokyo-comment :slant italic))))

;;;;; web-mode
         `(web-mode-error-face ((,class (:foreground ,tokyo-red-dark :underline t))))
         `(web-mode-warning-face ((,class (:foreground ,tokyo-yellow :underline t))))
         `(web-mode-preprocessor-face ((,class (:foreground ,tokyo-teal))))
         `(web-mode-block-delimiter-face ((,class (:foreground ,tokyo-dark5))))
         `(web-mode-block-control-face ((,class (:foreground ,tokyo-magenta))))
         `(web-mode-builtin-face ((,class (:foreground ,tokyo-cyan-bright))))
         `(web-mode-symbol-face ((,class (:foreground ,tokyo-orange))))
         `(web-mode-doctype-face ((,class (:foreground ,tokyo-comment))))
         `(web-mode-html-tag-face ((,class (:foreground ,tokyo-red))))
         `(web-mode-html-tag-custom-face ((,class (:foreground ,tokyo-red))))
         `(web-mode-html-tag-unclosed-face ((,class (:foreground ,tokyo-red :underline t))))
         `(web-mode-html-tag-namespaced-face ((,class (:foreground ,tokyo-red))))
         `(web-mode-html-tag-bracket-face ((,class (:foreground ,tokyo-fg-dark))))
         `(web-mode-html-attr-name-face ((,class (:foreground ,tokyo-magenta))))
         `(web-mode-html-attr-custom-face ((,class (:foreground ,tokyo-magenta :slant italic))))
         `(web-mode-html-attr-engine-face ((,class (:foreground ,tokyo-magenta))))
         `(web-mode-html-attr-equal-face ((,class (:foreground ,tokyo-fg-dark))))
         `(web-mode-html-attr-value-face ((,class (:foreground ,tokyo-green))))
         `(web-mode-block-attr-name-face ((,class (:foreground ,tokyo-magenta))))
         `(web-mode-block-attr-value-face ((,class (:foreground ,tokyo-green))))
         `(web-mode-variable-name-face ((,class (:foreground ,tokyo-fg))))
         `(web-mode-css-selector-face ((,class (:foreground ,tokyo-teal))))
         `(web-mode-css-selector-class-face ((,class (:foreground ,tokyo-green))))
         `(web-mode-css-selector-tag-face ((,class (:foreground ,tokyo-cyan-support))))
         `(web-mode-css-pseudo-class-face ((,class (:foreground ,tokyo-cyan-bright))))
         `(web-mode-css-at-rule-face ((,class (:foreground ,tokyo-magenta))))
         `(web-mode-css-property-name-face ((,class (:foreground ,tokyo-blue))))
         `(web-mode-css-color-face ((,class (:foreground ,tokyo-orange))))
         `(web-mode-css-priority-face ((,class (:foreground ,tokyo-orange :weight bold))))
         `(web-mode-css-function-face ((,class (:foreground ,tokyo-cyan))))
         `(web-mode-css-variable-face ((,class (:foreground ,tokyo-fg))))
         `(web-mode-function-name-face ((,class (:foreground ,tokyo-blue))))
         `(web-mode-filter-face ((,class (:foreground ,tokyo-cyan))))
         `(web-mode-function-call-face ((,class (:foreground ,tokyo-blue))))
         `(web-mode-string-face ((,class (:foreground ,tokyo-green))))
         `(web-mode-block-string-face ((,class (:foreground ,tokyo-green))))
         `(web-mode-part-string-face ((,class (:foreground ,tokyo-green))))
         `(web-mode-javascript-string-face ((,class (:foreground ,tokyo-green))))
         `(web-mode-css-string-face ((,class (:foreground ,tokyo-green))))
         `(web-mode-json-key-face ((,class (:foreground ,tokyo-teal))))
         `(web-mode-json-context-face ((,class (:foreground ,tokyo-magenta))))
         `(web-mode-json-string-face ((,class (:foreground ,tokyo-green))))
         `(web-mode-comment-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(web-mode-block-comment-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(web-mode-part-comment-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(web-mode-json-comment-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(web-mode-javascript-comment-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(web-mode-css-comment-face ((,class (:foreground ,tokyo-comment :slant italic))))
         `(web-mode-annotation-face ((,class (:foreground ,tokyo-comment))))
         `(web-mode-annotation-tag-face ((,class (:foreground ,tokyo-dark5))))
         `(web-mode-annotation-type-face ((,class (:foreground ,tokyo-cyan-bright))))
         `(web-mode-annotation-value-face ((,class (:foreground ,tokyo-green))))
         `(web-mode-annotation-html-face ((,class (:foreground ,tokyo-comment))))
         `(web-mode-constant-face ((,class (:foreground ,tokyo-orange))))
         `(web-mode-type-face ((,class (:foreground ,tokyo-cyan-bright))))
         `(web-mode-keyword-face ((,class (:foreground ,tokyo-magenta :slant italic))))
         `(web-mode-param-name-face ((,class (:foreground ,tokyo-fg-dark))))
         `(web-mode-whitespace-face ((,class (:foreground ,tokyo-red :background ,tokyo-diff-del-bg))))
         `(web-mode-inlay-face ((,class (:background ,tokyo-bg-dark))))
         `(web-mode-block-face ((,class (:background ,tokyo-bg-dark))))
         `(web-mode-part-face ((,class (:background ,tokyo-bg-dark))))
         `(web-mode-script-face ((,class (:background ,tokyo-bg-dark))))
         `(web-mode-style-face ((,class (:background ,tokyo-bg-dark))))
         `(web-mode-folded-face ((,class (:foreground ,tokyo-comment :underline t))))
         `(web-mode-current-element-highlight-face ((,class (:background ,tokyo-bg-highlight))))
         `(web-mode-current-column-highlight-face ((,class (:background ,tokyo-bg-line))))
         `(web-mode-comment-keyword-face ((,class (:foreground ,tokyo-magenta-hot :weight bold))))
         `(web-mode-sql-keyword-face ((,class (:foreground ,tokyo-cyan))))
         `(web-mode-html-entity-face ((,class (:foreground ,tokyo-orange))))

;;;;; auctex
         `(font-latex-bold-face ((,class (:foreground ,tokyo-fg :weight bold))))
         `(font-latex-italic-face ((,class (:foreground ,tokyo-fg :slant italic))))
         `(font-latex-math-face ((,class (:foreground ,tokyo-teal))))
         `(font-latex-script-char-face ((,class (:foreground ,tokyo-orange))))
         `(font-latex-sectioning-0-face ((,class (:foreground ,tokyo-heading1 :height ,h1 :weight bold))))
         `(font-latex-sectioning-1-face ((,class (:foreground ,tokyo-heading2 :height ,h2 :weight bold))))
         `(font-latex-sectioning-2-face ((,class (:foreground ,tokyo-heading3 :height ,h3 :weight bold))))
         `(font-latex-sectioning-3-face ((,class (:foreground ,tokyo-heading4 :weight bold))))
         `(font-latex-sectioning-4-face ((,class (:foreground ,tokyo-heading5 :weight bold))))
         `(font-latex-sectioning-5-face ((,class (:foreground ,tokyo-heading6 :weight bold))))
         `(font-latex-sedate-face ((,class (:foreground ,tokyo-fg-dark))))
         `(font-latex-slide-title-face ((,class (:foreground ,tokyo-blue :weight bold :height ,h1))))
         `(font-latex-string-face ((,class (:foreground ,tokyo-green))))
         `(font-latex-subscript-face ((,class (:height 0.9))))
         `(font-latex-superscript-face ((,class (:height 0.9))))
         `(font-latex-verbatim-face ((,class (:foreground ,tokyo-teal :inherit fixed-pitch))))
         `(font-latex-warning-face ((,class (:foreground ,tokyo-orange :weight bold))))
         `(font-latex-doctex-documentation-face ((,class (:background ,tokyo-bg-dark))))
         `(font-latex-doctex-preprocessor-face ((,class (:foreground ,tokyo-magenta))))

;;;;; centaur-tabs
         `(centaur-tabs-default ((,class (:background ,tokyo-bg-dark :foreground ,tokyo-comment))))
         `(centaur-tabs-selected ((,class (:background ,tokyo-bg :foreground ,tokyo-fg :weight bold))))
         `(centaur-tabs-unselected ((,class (:background ,tokyo-bg-dark :foreground ,tokyo-comment))))
         `(centaur-tabs-selected-modified ((,class (:background ,tokyo-bg :foreground ,tokyo-orange :weight bold))))
         `(centaur-tabs-unselected-modified ((,class (:background ,tokyo-bg-dark :foreground ,tokyo-orange))))
         `(centaur-tabs-active-bar-face ((,class (:background ,tokyo-blue))))
         `(centaur-tabs-modified-marker-selected ((,class (:foreground ,tokyo-orange))))
         `(centaur-tabs-modified-marker-unselected ((,class (:foreground ,tokyo-orange))))

;;;;; git-gutter
         `(git-gutter:added ((,class (:foreground ,tokyo-git-add))))
         `(git-gutter:deleted ((,class (:foreground ,tokyo-git-delete))))
         `(git-gutter:modified ((,class (:foreground ,tokyo-git-change))))
         `(git-gutter:unchanged ((,class (:background ,tokyo-bg-highlight))))

;;;;; git-gutter-fr
         `(git-gutter-fr:added ((,class (:foreground ,tokyo-git-add))))
         `(git-gutter-fr:deleted ((,class (:foreground ,tokyo-git-delete))))
         `(git-gutter-fr:modified ((,class (:foreground ,tokyo-git-change))))

;;;;; highlight-numbers
         `(highlight-numbers-number ((,class (:foreground ,tokyo-orange))))

;;;;; multiple-cursors
         `(mc/cursor-face ((,class (:inverse-video nil :background ,tokyo-bg-highlight :foreground ,tokyo-fg))))
         `(mc/cursor-bar-face ((,class (:background ,tokyo-fg :height 1))))
         `(mc/region-face ((,class (:inherit region))))

;;;;; neotree
         `(neo-banner-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(neo-header-face ((,class (:foreground ,tokyo-fg))))
         `(neo-root-dir-face ((,class (:foreground ,tokyo-blue :weight bold))))
         `(neo-dir-link-face ((,class (:foreground ,tokyo-blue))))
         `(neo-file-link-face ((,class (:foreground ,tokyo-fg))))
         `(neo-expand-btn-face ((,class (:foreground ,tokyo-comment))))
         `(neo-vc-default-face ((,class (:foreground ,tokyo-fg))))
         `(neo-vc-up-to-date-face ((,class (:foreground ,tokyo-fg))))
         `(neo-vc-edited-face ((,class (:foreground ,tokyo-git-change))))
         `(neo-vc-needs-merge-face ((,class (:foreground ,tokyo-orange))))
         `(neo-vc-added-face ((,class (:foreground ,tokyo-git-add))))
         `(neo-vc-conflict-face ((,class (:foreground ,tokyo-red :weight bold))))
         `(neo-vc-missing-face ((,class (:foreground ,tokyo-red))))
         `(neo-vc-ignored-face ((,class (:foreground ,tokyo-git-ignored))))

;;;;; nerd-icons
         `(nerd-icons-red ((,class (:foreground ,tokyo-red))))
         `(nerd-icons-lred ((,class (:foreground ,tokyo-red))))
         `(nerd-icons-dred ((,class (:foreground ,tokyo-red-dark))))
         `(nerd-icons-green ((,class (:foreground ,tokyo-green))))
         `(nerd-icons-lgreen ((,class (:foreground ,tokyo-green))))
         `(nerd-icons-dgreen ((,class (:foreground ,tokyo-teal-dark))))
         `(nerd-icons-yellow ((,class (:foreground ,tokyo-yellow))))
         `(nerd-icons-lyellow ((,class (:foreground ,tokyo-yellow))))
         `(nerd-icons-dyellow ((,class (:foreground ,tokyo-orange))))
         `(nerd-icons-blue ((,class (:foreground ,tokyo-blue))))
         `(nerd-icons-lblue ((,class (:foreground ,tokyo-cyan))))
         `(nerd-icons-dblue ((,class (:foreground ,tokyo-blue-dark))))
         `(nerd-icons-maroon ((,class (:foreground ,tokyo-red-dark))))
         `(nerd-icons-lmaroon ((,class (:foreground ,tokyo-red))))
         `(nerd-icons-dmaroon ((,class (:foreground ,tokyo-red-dark))))
         `(nerd-icons-purple ((,class (:foreground ,tokyo-magenta))))
         `(nerd-icons-lpurple ((,class (:foreground ,tokyo-magenta))))
         `(nerd-icons-dpurple ((,class (:foreground ,tokyo-magenta-dark))))
         `(nerd-icons-orange ((,class (:foreground ,tokyo-orange))))
         `(nerd-icons-lorange ((,class (:foreground ,tokyo-orange))))
         `(nerd-icons-dorange ((,class (:foreground ,tokyo-orange))))
         `(nerd-icons-cyan ((,class (:foreground ,tokyo-cyan))))
         `(nerd-icons-lcyan ((,class (:foreground ,tokyo-cyan-bright))))
         `(nerd-icons-dcyan ((,class (:foreground ,tokyo-teal-dark))))
         `(nerd-icons-pink ((,class (:foreground ,tokyo-red))))
         `(nerd-icons-lpink ((,class (:foreground ,tokyo-red))))
         `(nerd-icons-dpink ((,class (:foreground ,tokyo-red-dark))))
         `(nerd-icons-silver ((,class (:foreground ,tokyo-fg-dark))))
         `(nerd-icons-lsilver ((,class (:foreground ,tokyo-fg))))
         `(nerd-icons-dsilver ((,class (:foreground ,tokyo-comment))))

;;;;; perspective
         `(persp-selected-face ((,class (:foreground ,tokyo-blue :weight bold))))

;;;;; solaire
         `(solaire-default-face ((,class (:background ,tokyo-bg-dark :foreground ,tokyo-fg))))
         `(solaire-minibuffer-face ((,class (:background ,tokyo-bg-dark :foreground ,tokyo-fg))))
         `(solaire-hl-line-face ((,class (:background ,tokyo-bg-highlight))))
         `(solaire-line-number-face ((,class (:background ,tokyo-bg-dark :foreground ,tokyo-line-nr))))
         `(solaire-mode-line-face ((,class (:background ,tokyo-bg-dark))))
         `(solaire-mode-line-inactive-face ((,class (:background ,tokyo-bg-darkest))))
         `(solaire-org-hide-face ((,class (:foreground ,tokyo-bg-dark))))

;;;;; undo-tree
         `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,tokyo-fg :weight bold))))
         `(undo-tree-visualizer-current-face ((,class (:foreground ,tokyo-red))))
         `(undo-tree-visualizer-default-face ((,class (:foreground ,tokyo-comment))))
         `(undo-tree-visualizer-register-face ((,class (:foreground ,tokyo-yellow))))
         `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,tokyo-teal))))

;;;;; wgrep
         `(wgrep-face ((,class (:foreground ,tokyo-green :background ,tokyo-diff-add-bg))))
         `(wgrep-delete-face ((,class (:foreground ,tokyo-red :background ,tokyo-diff-del-bg))))
         `(wgrep-done-face ((,class (:foreground ,tokyo-blue))))
         `(wgrep-file-face ((,class (:foreground ,tokyo-comment))))
         `(wgrep-reject-face ((,class (:foreground ,tokyo-red :weight bold)))))

        (custom-theme-set-variables
         theme-name

;;;;; ansi-color
         `(ansi-color-names-vector
           [,tokyo-line-nr ,tokyo-red ,tokyo-teal ,tokyo-yellow
            ,tokyo-blue ,tokyo-magenta ,tokyo-cyan ,tokyo-line-nr-cur])

;;;;; pdf-view
         `(pdf-view-midnight-colors '(,tokyo-fg . ,tokyo-bg)))))))

;;; Hooks

(defcustom tokyo-night-after-load-hook nil
  "Hook run after a Tokyo theme is loaded.
Each function is called with the theme name (a symbol) as its
sole argument.  Useful for applying additional customizations
that depend on theme colors being set."
  :type 'hook
  :group 'tokyo-night)

;;; Palette API

(defun tokyo-night--palette-for (theme)
  "Return the colors alist for THEME, or nil if unknown."
  (pcase theme
    ('tokyo-night tokyo-night-colors-alist)
    ('tokyo-night-storm tokyo-night-storm-colors-alist)
    ('tokyo-night-moon  tokyo-night-moon-colors-alist)
    ('tokyo-night-day   tokyo-night-day-colors-alist)))

(defun tokyo-night-get-color (name &optional theme)
  "Return the hex color value for NAME in the current Tokyo theme.
NAME is a string like \"tokyo-blue\".  If THEME is given, look up
colors in that variant's palette instead.  User overrides from
`tokyo-night-override-colors-alist' are respected."
  (let* ((variant (or theme tokyo-night--current))
         (palette (or (tokyo-night--palette-for variant)
                      (error "No Tokyo theme is active")))
         (merged (append tokyo-night-override-colors-alist palette)))
    (cdr (assoc name merged))))

;;;###autoload
(defmacro tokyo-night-with-colors (&rest body)
  "Bind all palette colors for the current Tokyo theme and evaluate BODY.
Inside BODY, each palette color is available as a local variable,
e.g. `tokyo-blue', `tokyo-bg', etc.

Example:
  (tokyo-night-with-colors
    (set-face-attribute \\='some-face nil :foreground tokyo-blue))"
  (declare (indent 0))
  `(let* ((--tokyo-palette
           (append tokyo-night-override-colors-alist
                   (or (tokyo-night--palette-for tokyo-night--current)
                       (error "No Tokyo theme is active"))))
          ,@(mapcar (lambda (entry)
                      `(,(intern (car entry))
                        (cdr (assoc ,(car entry) --tokyo-palette))))
                    tokyo-night-colors-alist))
     ,@body))

;;; Interactive Palette Viewer

(defun tokyo-night-list-colors (&optional theme)
  "Display all palette colors for the current Tokyo theme.
With prefix argument, prompt for THEME variant."
  (interactive
   (list (when current-prefix-arg
           (intern (completing-read "Variant: "
                                    (mapcar #'symbol-name tokyo-night--variants)
                                    nil t)))))
  (let* ((variant (or theme tokyo-night--current
                      (error "No Tokyo theme is active")))
         (palette (or (tokyo-night--palette-for variant)
                      (error "Unknown theme: %s" variant)))
         (merged (append tokyo-night-override-colors-alist palette))
         (buf (get-buffer-create (format "*Tokyo Palette: %s*" variant))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Palette for %s\n\n" variant))
        (dolist (entry merged)
          (let ((name (car entry))
                (color (cdr entry)))
            (insert (format "  %-25s  %s  " name color))
            (insert (propertize "  sample  "
                                'face `(:foreground ,color)))
            (insert (propertize "  sample  "
                                'face `(:background ,color
                                        :foreground ,(if (< (tokyo-night--relative-luminance color) 0.5)
                                                         "#ffffff" "#000000"))))
            (insert "\n")))
        (goto-char (point-min)))
      (special-mode))
    (pop-to-buffer buf)))

(defun tokyo-night--relative-luminance (hex)
  "Return the relative luminance of HEX color string.
Uses the WCAG 2.0 formula."
  (let* ((rgb (color-name-to-rgb hex))
         (r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb))
         (adjust (lambda (c)
                   (if (<= c 0.03928)
                       (/ c 12.92)
                     (expt (/ (+ c 0.055) 1.055) 2.4)))))
    (+ (* 0.2126 (funcall adjust r))
       (* 0.7152 (funcall adjust g))
       (* 0.0722 (funcall adjust b)))))

;;; User Commands

(defvar tokyo-night--current nil
  "The currently active Tokyo theme, or nil.")

(defconst tokyo-night--variants
  '(tokyo-night tokyo-night-storm tokyo-night-moon tokyo-night-day)
  "List of all Tokyo theme variants.")

;;;###autoload
(defun tokyo-night-reload ()
  "Reload the current Tokyo theme.
Useful after changing `tokyo-night-override-colors-alist' or
`tokyo-night-scale-headings' without having to call `load-theme'
manually."
  (interactive)
  (if tokyo-night--current
      (progn
        (load-theme tokyo-night--current t)
        (run-hook-with-args 'tokyo-night-after-load-hook tokyo-night--current))
    (user-error "No Tokyo theme is currently active")))

;;;###autoload
(defun tokyo-night-select ()
  "Select and load a Tokyo theme variant interactively."
  (interactive)
  (let* ((names (mapcar #'symbol-name tokyo-night--variants))
         (choice (intern (completing-read "Tokyo theme: " names nil t))))
    (mapc #'disable-theme tokyo-night--variants)
    (load-theme choice t)
    (setq tokyo-night--current choice)
    (run-hook-with-args 'tokyo-night-after-load-hook choice)))

(defun tokyo-night--set-current (theme)
  "Record THEME as the active Tokyo theme.
Called from `enable-theme-functions'."
  (when (memq theme tokyo-night--variants)
    (setq tokyo-night--current theme)))

(defun tokyo-night--clear-current (theme)
  "Clear the active Tokyo theme if THEME is being disabled.
Called from `disable-theme-functions'."
  (when (eq theme tokyo-night--current)
    (setq tokyo-night--current nil)))

(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'tokyo-night--set-current)
  (add-hook 'disable-theme-functions #'tokyo-night--clear-current))

(provide 'tokyo-night)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; tokyo-night.el ends here
