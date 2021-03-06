(defun dot-prin1 (list)
  (cond ((atom list) (format t "~s" list))
        (t (format t "(")
           (dot-prin1 (car list))
           (format t " . ")
           (dot-prin1 (cdr list))
           (format t ")"))))

;;; will print infinitely long flat list, whose only elements are foo
;;; will print infinitely car-deep list, cdrs at any level
;;; (which are foo) will never be printed(only left parentheses will be printed)

(let ()
  (setf circ1 (list 'foo))
  (setf (cdr circ1) circ1)

  (setf circ2 (cons 'foo 'foo))
  (setf (car circ2) circ2)
  'done) ; to prevent the infinite printing

(defun hybrid-prin1 (list)
  (labels ((print-elements (list)
             (cond ((null list) nil)
                   ((atom list)
                    (format t ". ~s" list))
                   (t (hybrid-prin1 (car list))
                      (if (cdr list)
                          (format t " "))
                      (print-elements (cdr list))))))
    (cond ((atom list)
           (format t "~s" list))
          (t (format t "(")
             (print-elements list)
             (format t ")")))))

;;; state-of-art
(defun hybrid-prin1 (list)
  (labels ((print-elements (list)
             (cond ((atom list) ; never nil
                    (format t ". ~s" list))
                   (t (hybrid-prin1 (car list))
                      (cond ((cdr list)
                             (format t " ")
                             (print-elements (cdr list))))))))
    (cond ((atom list)
           (format t "~s" list))
          (t (format t "(")
             (print-elements list)
             (format t ")")))))