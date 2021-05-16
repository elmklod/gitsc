(defun laugh (n)
  (if (zerop n)
      nil
      (cons 'ha (laugh (- n 1)))))