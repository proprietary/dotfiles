;; Formats SQL in a region or in a buffer.
;;
;; Requires sqlformat(1), which you can install with:
;; $ pipx install sqlparse
;;

(defun zelcon/format-sql-region (start end)
  (interactive "r")
  (if (executable-find "sleek")
      (shell-command-on-region
       start end
       "sleek --uppercase true"
       nil t nil)
    (error "`sqlformat` not found on path")))
