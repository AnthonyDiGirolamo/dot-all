;;; tokyo-night-day-theme.el --- Tokyo Night theme (day variant) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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

;; The "day" variant of Tokyo Night -- a light theme.
;; See `tokyo-night.el' for the shared infrastructure.

;;; Code:

(require 'tokyo-night)

(deftheme tokyo-night-day "A clean light theme inspired by Tokyo city lights (day variant).")

(tokyo-night--apply-theme 'tokyo-night-day tokyo-night-day-colors-alist)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'tokyo-night-day)

(provide 'tokyo-night-day-theme)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; tokyo-night-day-theme.el ends here
