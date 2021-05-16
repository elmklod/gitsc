(defun find-first-odd (numlist)
  (dolist (e numlist)
    (format t "~&Testing ~s..." e)
    (when (oddp e)
      (format t "found an odd number.")
      (return e))))      
    

(defun check-all-odd (numlist)
  (dolist (e numlist t)
    (format t "~&Checking ~s..." e)
    (if (evenp e)
        (return))))
;;;

(defun it-member (e list)
  (dolist (i list)
    (if (eql i e)
        (return t))))

(defun it-assoc (key alist)
  (dolist (entry alist)
    (if (eql key (car entry))
        (return entry))))

(defun check-all-odd (numlist)
  (if (null numlist)
      t
      (let ((e (first numlist)))
        (format t "~&Checking ~s..." e)
        (if (oddp e)
            (check-all-odd (rest numlist))))))
