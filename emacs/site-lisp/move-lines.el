(defmacro zelcon/save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'zelcon/save-column 'lisp-indent-function 0)

(defun zelcon/move-line-up ()
  (interactive)
  (zelcon/save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun zelcon/move-line-down ()
  (interactive)
  (zelcon/save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(provide 'move-lines)
