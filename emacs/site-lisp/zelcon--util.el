(defun zelcon/odds-to-prob (odds)
  "Convert American betting odds (e.g., +200, -910) to mathematical probabilities."
  (if (> odds 0)
      (/ 100.0 (+ odds 100.0))
    (/ (* -1.0 odds) (+ (* -1.0 odds) 100.0))))

(defun zelcon/generate-random-hex-array (length)
  "Generate an array of LENGTH random hexadecimal bytes."
  (interactive "nArray length: ")
  (let* ((random-bytes (cl-loop for i from 1 to length
                              collect (random 256)))
         (hex-strings (mapcar (lambda (byte) (format "0x%02X" byte)) random-bytes))
         (array-content (mapconcat 'identity hex-strings ", ")))
    array-content))

(provide 'zelcon--util)
