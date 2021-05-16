(defun make-set (list)
 "Makes a set with unique elements from a list"
 (cond ((null list) nil) ; or list
       ((my-member (first list) (rest list))
	(make-set (rest list)))
       (t (cons (first list)
		(make-set (rest list))))))

(defun my-member (element list)
  "Checks if the element belongs to the list, if it does, returns a sublist beginning with this element, otherwise returns nil"
  (cond ((null list) nil) ; or list
	((equal element (first list))
	 list)
	(t (my-member element (rest list)))))

(defun set-equal (set1 set2)
  "Checks if two sets are equal"
  (and (my-subsetp set1 set2)
       (my-subsetp set2 set1)))

(defun my-member (element list)
  "Checks if the element belongs to the list, if it does, returns a sublist beginning with this element, otherwise returns nil"
  (cond ((null list) nil) ; or list
        ((if (and (listp element)
		  (listp (first list)))
	     (set-equal element (first list))
	     (equal element (first list))
         list)
        (t (my-member element (rest list)))))

(defun my-intersection (set1 set2)
  "Makes an intersection of two sets"
 (cond ((or (null set1)
	    (null set2))
	nil)
       ((my-member (first set1) set2)
	(cons (first set1)
	      (my-intersection (rest set1)
			    set2)))
       (t (my-intersection (rest set1) set2))))

(defun my-union (set1 set2)
  "Makes a union of two sets"
 (cond ((null set1)
	set2)
       ((null set2) ; not required, makes less copies, takes some time. Not clear what is faster
	set1)
       ((my-member (first set1) set2)
	(my-union (rest set1) set2))
       (t (cons (first set1)
		(my-union (rest set1) set2)))))

(defun my-set-difference (set1 set2)
  "Performs second set subtraction from the first. The result is a set."
  (cond ((null set1)
	 nil)
	((null set2)
	 set1)
	((my-member (first set1) set2)
	 (my-set-difference (rest set1) set2))
	(t (cons (first set1)
		 (my-set-difference (rest set1)
				 set2)))))

;;;(defun my-set-difference (set1 set2) "Performs the second set subtraction from the first. The result is a set"
;;; (mapcar (lambda (element) (setf set1 (remove element set1))) set2) set1)
;;;(defun my-set-difference (set1 set2)
;;; (reduce (lambda (set element) (remove element set))
;;;         set2
;;;         :initial-value set1))
(defun my-subsetp (set1 set2)
  "Checks if the first set is a subset of the second"
  (null (my-set-difference set1 set2)))

(defun my-subsetp2 (set1 set2)
  "Checks if the first set is a subset of the second"
  (or (null set1)
      (and (member (first set1)
		   set2)
	   (my-subsetp2 (rest set1)
			set2))))
(defun proper-subsetp (set1 set2)
  "Tests if first set is the proper subset of the second(subset but not set-equal)"
  (and (my-subsetp set1 set2)
       (not (my-subsetp set2 set1))))

(defun my-set-exclusive-or
  (set1 set2)
  "Makes the symmetric difference of two sets"
  (my-union
    (my-set-difference
     set1
     set2)
    (my-set-difference
     set2
     set1)))

;;; (my-set-diffrence ; it is unclear which one is generally faster
;;;  (my-union set1 set2)
;;;  (my-intersection set1 set2))



