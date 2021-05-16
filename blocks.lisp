(setf database '((b1 shape brick)
                 (b1 color green)
                 (b1 size small)
                 (b1 supported-by b2)
                 (b1 supported-by b3)
                 (b2 shape brick)
                 (b2 color red)
                 (b2 size small)
                 (b2 supports b1)
                 (b2 left-of b3)
                 (b3 shape brick)
                 (b3 color red)
                 (b3 size small)
                 (b3 supports b1)
                 (b3 right-of b2)
                 (b4 shape pyramid)
                 (b4 color blue)
                 (b4 size large)
                 (b4 supported-by b5)
                 (b5 shape cube)
                 (b5 color green)
                 (b5 size large)
                 (b5 supports b4)
                 (b6 shape brick)
                 (b6 color purple)
                 (b6 size large)))

(defun match-element (element pattern)
  (or (eq element pattern) (eq pattern '?))) ; element is known to always be a symbol

(defun match-triple (triple triple-pattern)
  (and (match-element
	(first triple)
	(first triple-pattern))
       (match-element
	(second triple)
	(second triple-pattern))
       (match-element
	(third triple)
	(third triple-pattern))))

(defun match-triple (triple triple-pattern)
  (reduce (let ((n 0))
	    (lambda (val element)
	      (and val
		   (let ((new-val 
			  (match-element 
			   element
		    	   (nth n triple-pattern))))
		     (setf n (+ n 1))
		     new-val))))
	  triple
	  :initial-value t))

(defun match-triple (assertion triple-pattern)
  (every #'match-element
	 assertion
	 triple-pattern))

(defun fetch (pattern)
  (remove-if-not
   (lambda (assertion)
    (match-triple
     assertion
     pattern))
   database))

(fetch '(b4 shape ?))
(fetch '(? shape brick))
(fetch '(b2 ? b3))
(fetch '(? color ?))
(fetch '(b4 ? ?)) ; if there are no converses in the database, do (append (fetch '(b4 ? ?))
;;;                    (fetch '(? ? b4))
;;;         otherwise don't

(defun color-pattern (block)
 (list block 'color '?))

(defun supporters (block)
  (mapcar #'first
	  (fetch (list '? 'supports block))))

(defun supp-cube (block)
  (find-if ; it is more concise, but less reliable 
   (lambda (supporter) ; as the block symbol can be nil
     (fetch (list supporter 'shape 'cube))) ; although it would lead to unnatural workarounds.
   (supporters block)))

;;; The book version won't fail if one of the blocks is nil
(defun supp-cube (block)
  (member 'cube
	  (mapcar (lambda (b)
	           (third
         	    (first (fetch (list b 'shape '?)))))
	          (supporters block))))

(defun supp-cube (block)       
  (find-if ; it is more concise
   (lambda (supporter)                        
     (if (fetch (list supporter 'shape 'cube))
         (return-from supp-cube t))) ; explicitly returns, avoidinig the problem      
   (supporters block)))


(defun desc1 (block)
  (fetch (list block '? '?)))

(defun desc2 (block)
  (mapcar #'rest (desc1 block)))

(defun description (block)
  (reduce #'append (desc2 block)))

;;; add a new attribute 'material'

(setf database
      (append '((b1 material wood)
		(b2 material plastic))
	      database))
;;; more concise but creates 2 extra cons cells to be GCed. Still preffered, as the reader creates more for the following expression during the read phase
;(cons               
;  '(b1 material wood)
;  (cons '(b2 material plastic)
;        database))
