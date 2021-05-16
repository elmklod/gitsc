(defun tr-count-slices (loaf)
  (labels ((rec (loaf result)
             (cond ((null loaf) result)
                   (t (rec (rest loaf) (+ 1 result))))))
    (rec loaf 0)))

(defun tr-reverse (list)
  (labels ((rec (list result)
             (cond ((null list) result)
                   (t (rec (rest list)
                           (cons (first list) result))))))
    (rec list nil)))