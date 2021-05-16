;;; A recursive function calls itself either directly in its body or indirectly via the body of functions that are called during its body evaluation

(defun every-other (list)
  (cond ((null list) nil)
        (t (cons (first list)
                 (every-other (rest (rest list)))))))

(defun left-half (list)
  (left-half-rec list (/ (length list) 2)))

(defun left-half-rec (list len)
  (cond ((<= len 0) nil)
        (t (cons (first list)
                 (left-half-rec
                  (rest list)
                  (- len 1))))))

(defun merge-lists (list1 list2)
  (cond ((null list1)
         list2)
        ((null list2)
         list1)
        (t (let ((e1 (first list1))
                 (e2 (first list2)))
             (if (> e1 e2)
                 (cons e2
                       (merge-lists list1
                                    (rest list2)))
                 (cons e1
                       (merge-lists (rest list1)
                                    list2)))))))


;;; recursive
;;; for n = 0 it returns 1
;;; for n < 0 it raises an division by 0 error
;;; for n > 0 it never terminates
;;; the journey is not smaller.
(defun factorial (n)
  (cond ((zerop n) 1)
        (t (/ (factorial (+ n 1))
              (+ n 1)))))
