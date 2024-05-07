(defun zelcon/uncamelize-region (start end)
  "Replace a camel case symbol with a snake_case symbol."
  (interactive "r")
  (let ((case-fold-search nil))
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\\([a-z]\\)\\([A-Z]\\)" nil t)
        (replace-match "\\1_\\2")
        (downcase-region (match-beginning 0) (match-end 0))))))

(defun zelcon/uncamelize-symbol-at-point ()
  "Replace a camel case symbol at point with a snake_case symbol."
  (interactive)
  (let ((case-fold-search nil))
    (save-excursion
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (when bounds
          (zelcon/uncamelize-region (car bounds) (cdr bounds)))))))
