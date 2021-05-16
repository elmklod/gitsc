(defun space-over (n)
  (cond ((< n 0) (format t "~&Error!~%"))
        ((zerop n) nil)
        (t (format t " ")
           (space-over (- n 1)))))

(defun test (n)
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~a~%" plotting-string))

(defun plot-points (string y-list)
  (mapcar (lambda (y-val)
            (plot-one-point
             string y-val))
          y-list))

(defun generate (m n)
  (if (> m n)
      nil
      (cons m (generate (+ m 1) n))))

;;; can be macroed
(defun make-graph nil
  (let* ((func (or (format t
                           "~&Function to graph? ")
                   (finish-output)
                   (read)))
         (start (or (format t
                            "~&Starting x value? ")
                    (finish-output)
                    (read)))
         (end (or (format t
                          "~&Ending x value? ")
                  (finish-output)
                  (read)))
         (plotting-string (or (format t
                                      "~&Plotting string? ")
                              (finish-output)
                              (read))))
    (format t "~&")
    (plot-points plotting-string
                 (mapcar func (generate start end)))))
;;; improved version. The book reminded to consolidate duplicate patterns into functions
(defun prompt-for (prompt)
  (format t prompt)
  (finish-output)
  (read))

(defun make-graph nil
  (let* ((func (prompt-for "~&Function to graph? "))
         (start (prompt-for "~&Starting x value? "))
         (end (prompt-for "~&Ending x value? "))
         (plotting-string (prompt-for "~&Plotting string? ")))
    (format t "~&")
    (plot-points plotting-string
                 (mapcar func (generate start end)))
    t))

(defun square (n) (* n n))