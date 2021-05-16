(defun beautiful (x y)
  (let* ((max (max x y))
         (avg (/ (+ x y) 2.0))
         (pct (* 100 (/ avg max))))
    (list 'average avg 'is
          pct 'percent 'of 'max max)))

;;; (setf *print-circle* t) to see the truth
;;; (setf x nil)
;;; (push x x) => (nil)
;;; (push x x) => (#1=(nil) . #1#) = ((nil) nil)
;;;             = (#1=(NIL) . #1#)
;;; (push x x) => (#1=(#2=(nil) . #2#) . #1#) = (((nil) nil) (nil) nil)
;;;             = (#2=(#1=(NIL) . #1#) . #2#)

;;; nothing
;;; It is clear that from now on that (length x) is 3
;;; This's set function original behavior
;;; It is possible to define such a setf form 
;;; but
;;; What happens to x is semantically unclear in most cases
;;; If it has less than three elements, should
;;; some noise like nil be pushed into x until its length is 3?
;;; If it has 3 elements, nothing changes.
;;; The whole expression is redundant
;;; If it has more than 3 elements, should its elements be popped until
;;; there are exactly 3?
;;; What to do with cdr-circular lists?