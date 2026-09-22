;;; transient-posframe.el --- Using posframe to show transient  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Yanghao Xie

;; Author: Yanghao Xie
;; Maintainer: Yanghao Xie <yhaoxie@gmail.com>
;; URL: https://github.com/yanghaoxie/transient-posframe
;; Package-Version: 20241212.940
;; Package-Revision: 1eb4ed61ad9f
;; Keywords: convenience, bindings, tooltip
;; Package-Requires: ((emacs "26.1") (posframe "1.4.4") (transient "0.8.2"))

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

;; Display transient popups using a posframe.
;; Check out the README for more information.

;;; Code:
(require 'posframe)
(require 'transient)

(defgroup transient-posframe nil
  "Using posframe to show transient popups"
  :group 'transient
  :prefix "transient-posframe")

(defcustom transient-posframe-font nil
  "The font used by transient-posframe.
When nil, use current frame's font as fallback."
  :group 'transient-posframe
  :type '(choice string (const :tag "Use font of current frame")))

(defcustom transient-posframe-poshandler #'posframe-poshandler-frame-center
  "The poshandler of transient-posframe."
  :group 'transient-posframe
  :type 'function)

(defcustom transient-posframe-border-width 1
  "The border width used by transient-posframe.
When 0, no border is showed."
  :group 'transient-posframe
  :type 'number)

(defcustom transient-posframe-parameters nil
  "The frame parameters used by transient-posframe."
  :group 'transient-posframe
  :type '(alist :key-type symbol :value-type sexp))

(defface transient-posframe
  '((t (:inherit default)))
  "Face used by the transient-posframe."
  :group 'transient-posframe)

(defface transient-posframe-border
  '((t (:inherit default :background "gray50")))
  "Face used by the transient-posframe's border."
  :group 'transient-posframe)

(defvar transient-posframe-display-buffer-action--previous nil
  "The previous value of `transient-display-buffer-action'.")

(defun transient-posframe--show-buffer (buffer _alist)
  "Show BUFFER in posframe and we do not use _ALIST at this period."
  (unless (posframe-workable-p)
    (error "Posframe is not workable"))
  (posframe-show
   buffer
   :font transient-posframe-font
   :position (point)
   :poshandler transient-posframe-poshandler
   :background-color (face-attribute 'transient-posframe :background nil t)
   :foreground-color (face-attribute 'transient-posframe :foreground nil t)
   :min-width transient-minimal-frame-width
   :internal-border-width transient-posframe-border-width
   :internal-border-color (face-attribute 'transient-posframe-border
                                          :background nil t)
   :override-parameters transient-posframe-parameters)
  (get-buffer-window transient--buffer-name t))

;;;###autoload
(define-minor-mode transient-posframe-mode
  "Toggle transient posframe mode on of off."
  :group 'transient-posframe
  :global t
  :lighter nil
  (cond
   (transient-posframe-mode
    (setq transient-posframe-display-buffer-action--previous
          transient-display-buffer-action)
    (setq transient-display-buffer-action
          '(transient-posframe--show-buffer)))
   (t
    (setq transient-display-buffer-action
          transient-posframe-display-buffer-action--previous))))

(provide 'transient-posframe)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; transient-posframe.el ends here
