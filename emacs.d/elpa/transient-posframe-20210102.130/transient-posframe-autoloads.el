;;; transient-posframe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "transient-posframe" "transient-posframe.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from transient-posframe.el

(defvar transient-posframe-mode nil "\
Non-nil if Transient-Posframe mode is enabled.
See the `transient-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `transient-posframe-mode'.")

(custom-autoload 'transient-posframe-mode "transient-posframe" nil)

(autoload 'transient-posframe-mode "transient-posframe" "\
Toggle transient posframe mode on of off.

If called interactively, enable Transient-Posframe mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "transient-posframe" '("transient-posframe-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; transient-posframe-autoloads.el ends here
