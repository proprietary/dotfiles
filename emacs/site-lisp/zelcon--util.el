(defun zelcon/odds-to-prob (odds)
  "Convert American betting odds (e.g., +200, -910) to mathematical probabilities."
  (if (> odds 0)
      (/ 100.0 (+ odds 100.0))
    (/ (* -1.0 odds) (+ (* -1.0 odds) 100.0))))

(provide 'zelcon--util)
