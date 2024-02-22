(defun zelcon/move-region (n)
  (let ((beg) (end) (keep))
    (if mark-active 
        (save-excursion
          (setq keep t)
          (setq beg (region-beginning)
                end (region-end))
          (goto-char beg)
          (setq beg (line-beginning-position))
          (goto-char end)
          (setq end (line-beginning-position 2)))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))
    (let ((offset (if (and (mark t) 
                           (and (>= (mark t) beg)
                                (< (mark t) end)))
                      (- (point) (mark t))))
          (rewind (- end (point))))
      (goto-char (if (< n 0) beg end))
      (forward-line n)
      (insert (delete-and-extract-region beg end))
      (backward-char rewind)
      (if offset (set-mark (- (point) offset))))
    (if keep
        (setq mark-active t
              deactivate-mark nil))))

;;;###autoload
(defun zelcon/move-region-up (n)
  "move the line(s) spanned by the active region up by N lines."
  (interactive "*p")
  (zelcon/move-region (- (or n 1))))

;;;###autoload
(defun zelcon/move-region-down (n)
  "move the line(s) spanned by the active region down by N lines."
  (interactive "*p")
  (zelcon/move-region (or n 1)))

(provide 'move-region)
