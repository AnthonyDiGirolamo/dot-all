;;; kanban-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "kanban" "kanban.el" (22777 4852 0 0))
;;; Generated autoloads from kanban.el

(autoload 'kanban-headers "kanban" "\
Fill the headers of your table with your org-mode TODO
states. If the table is too narrow, the only the first n TODO
states will be shown, with n as the number of columns in your
table.

Only not already present TODO states will be filled into empty
fields starting from the current column. All columns left of
the current one are left untouched.

Optionally ignore fields in columns left of STARTCOLUMN

\(fn &optional STARTCOLUMN)" nil nil)

(autoload 'kanban-zero "kanban" "\
Zero-state Kanban board: This Kanban board just displays all
org-mode headers which have a TODO state in their respective TODO
state. Useful for getting a simple overview of your tasks.

Gets the ROW and COLUMN via TBLFM ($# and @#) and can get a string as MATCH to select only entries with a matching tag, as well as a list of org-mode files as the SCOPE to search for tasks.

\(fn ROW COLUMN &optional MATCH SCOPE)" nil nil)

(autoload 'kanban-todo "kanban" "\
Kanban TODO item grabber. Fills the first column of the kanban
table with org-mode TODO entries, if they are not in another cell
of the table. This allows you to set the state manually and just
use org-mode to supply new TODO entries.

Gets the ROW and all other CELS via TBLFM ($# and @2$2..@>$>) and can get a string as MATCH to select only entries with a matching tag, as well as a list of org-mode files as the SCOPE to search for tasks.

\(fn ROW CELS &optional MATCH SCOPE)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; kanban-autoloads.el ends here
