;;; ox-rst-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ox-rst" "ox-rst.el" (0 0 0 0))
;;; Generated autoloads from ox-rst.el

(autoload 'org-rst-export-as-rst "ox-rst" "\
Export current buffer to a reStructuredText buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org RST Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-rst-convert-region-to-rst "ox-rst" "\
Assume the current region has Org syntax, and convert it to
reStructuredText.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in a Markdown buffer and use this command
to convert it." t nil)

(autoload 'org-rst-export-to-rst "ox-rst" "\
Export current buffer to a reStructuredText file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-rst-publish-to-rst "ox-rst" "\
Publish an org file to reStructuredText.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-rst" '("my-org-export-inline-image-p" "org-rst-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ox-rst-autoloads.el ends here
