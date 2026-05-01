;;; tokyo-night-storm-theme.el --- Tokyo Night theme (storm variant) -*- lexical-binding: t; -*-

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

;; The "storm" variant of Tokyo Night -- medium dark background.
;; See `tokyo-night.el' for the shared infrastructure.

;;; Code:

(require 'tokyo-night)

(deftheme tokyo-night-storm "A clean dark theme inspired by Tokyo city lights (storm variant).")

(tokyo-night--apply-theme 'tokyo-night-storm tokyo-night-storm-colors-alist)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'tokyo-night-storm)

(provide 'tokyo-night-storm-theme)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; tokyo-night-storm-theme.el ends here
