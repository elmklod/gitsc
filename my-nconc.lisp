(defun my-nconc (list object)
  (cond ((null list) object)
        ((null (cdr (last list)))
         (setf (cdr (last list)) object)
         list)
        (t (error "The first argument is not a proper list: ~s" list))))

(defun my-nconc (list object)
  (if (null list)
      object
      (progn
        (setf (cdr (last list)) object)
        list)))

(defun my-nconc (list object)
  (cond ((null list) object)
        (t (setf (cdr (last list)) object)
           list)))