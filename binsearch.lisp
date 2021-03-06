(defun binsearch (target numlist)
  (labels ((rec (beg end)
             (let* ((mid (floor (/ (+ beg end) 2)))
                    (midnum (nth mid numlist)))
               (cond ((< midnum target)
                      (rec (+ mid 1) end))
                     ((> midnum target)
                      (rec beg (- mid 1)))
                     (t mid)))))
    (rec 0 (- (length numlist) 1))))

;;; to view all steps
(defun binsearch (target numlist)
  (labels ((rec (beg end)
             (let* ((mid (floor (/ (+ beg end) 2)))
                    (midnum (nth mid numlist)))
               (cond ((< midnum target)
                      (rec (+ mid 1) end))
                     ((> midnum target)
                      (rec beg (- mid 1)))
                     (t (break "gotcha") mid)))))
    (rec 0 (- (length numlist) 1))))