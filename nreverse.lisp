;;; v1 mutates cdrs, v2 - cars

(defun nreverse-v1 (list) ;;; bug-ridden
  (cond ((null list) nil)
        (t (let ((reversed-tail (nreverse-v1 (cdr list))))
             (if reversed-tail
                 (setf (cdr (last reversed-tail))
                       list)
                 (setf reversed-tail list))
             reversed-tail))))

(defun nreverse-v1 (list) ;;; correct
  (cond ((null list) nil)
        (t (let ((reversed-tail (nreverse-v1 (cdr list))))
             (setf (cdr list) nil)
             (if reversed-tail
                 (setf (cdr (last reversed-tail))
                       list)
                 (setf reversed-tail list))
             reversed-tail))))

;;; want to write an iterative one
;;; and the one that utilizes tco using internal functions


;;; v2 swaps elements, not cons cells

;;; It implicitly swaps the cars
(defun swap-elements (n1 n2 list)
  (let ((val1 (nth n1 list)))
    (setf (nth n1 list) (nth n2 list)
          (nth n2 list) val1)))

;;; v2 well-behaves and reverses the shared structure correctly without causing nonsensical one-element lists, unlike v1 which is sadly the default
(defun nreverse-v2 (list)
  (if list
      (let* ((max-n (- (length list) 1))
             (limit (/ max-n 2)))
        (labels ((rec (count)
                   (let ((diff (- max-n count)))
                     (cond ((<= diff limit) nil)
                           (t (swap-elements count diff list)
                              (rec (+ 1 count)))))))
          (rec 0)
          list))))
