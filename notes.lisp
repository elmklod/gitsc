(setf note-table
 '((c 1) (c-sharp 2)
   (d 3) (d-sharp 4)
   (e 5)
   (f 6) (f-sharp 7)
   (g 8) (g-sharp 9)
   (a 10) (a-sharp 11)
   (b 12)))

(setf mary-lamb '(e d c d e e e))

(defun numbers (notes)
  (mapcar (lambda (note)
	    (second
	     (assoc note note-table)))
	  notes))

(defun notes (numbers)
  (mapcar (lambda (number)
	   (first
	    (rassoc (list number)
		    note-table
		    :test #'equal)))
	  numbers))

(defun raise (n numbers)
 (mapcar (lambda (number)
	  (+ n number))
	 numbers))

(defun normalize (numbers)
 (mapcar (lambda (number)
	  (let ((res (rem number 12)))
	   (if (zerop res)
	       12
	       res)))
	 numbers))

(defun transpose (n song)
 (notes (normalize (raise n (numbers song)))))
