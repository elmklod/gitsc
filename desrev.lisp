(defun destructive-reverse (list)
   "Destructively reverses the given list"
   (labels ((rec (list result)
             (if (null list) ; change null for atom and it will reverse non-list cons structures and don't forget to set (result last) to the atomic cdr of the list.
                 result                  
                 (let ((next (cdr list)))
                 (setf (cdr list) result)
                 (rec next list)))))
    (rec list nil)))
