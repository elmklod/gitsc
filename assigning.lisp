(let ((total-glasses 0))
  (defun sell (n)
    "Ye Olde Lemonade Stand: Sales by the Glass."
    (incf total-glasses n)
    (format t "~&That makes ~s glasses so far today."
            total-glasses)))

(let ((friends nil)
      (times-met nil))
  (defun meet (person)
    (cond ((eq person (first friends))
           (format t "~&We just met. It's been ~s times already."
                   (incf (cdr (assoc person times-met)))))
          ((member person friends)
           (format t "~&We know each other. We've met ~s times."
                   (incf (cdr (assoc person times-met)))))
          (t (push person friends)
             (push (cons person 1) times-met)
             'pleased-to-meet-you)))
  (defun forget (person)
    (cond ((member person friends)
           (setf friends (remove person friends)) ; setf would be unnecessary if I had access to delete function
           (setf times-met (remove-if (lambda (pair)
                                        (eq (car pair) person))
                                      times-met))
           'we-are-friends-no-more)
          (t (format t "~&No ~s to forget about"
                     person)))))

