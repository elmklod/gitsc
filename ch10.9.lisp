;;; This version is more appropriate than book's.
(defun add (prop obj)
  (let ((entry (assoc obj *things*)))
    (if entry
        (nconc (assoc obj *things*)
               (list prop))
        (push (list obj prop) *things*))))

(defun chop-all-but-last (list)
  (cond ((null list) nil)
        (t (setf (car list) (car (last list))
                 (cdr list) (cdr (last list)))
           list))) ; or just nil for the cdr if the lists are always proper

(defun chop (list)
  (cond ((null list) nil)
        (t (setf (cdr list) nil) ; nil may be substituted with (cdr (last list)) to preserve the non-nil atom at the end
           list)))

(defun chop (x)
  (if (consp x) (setf (cdr x) nil))
  x)

;;; as nil is never supposed to be choped or tacked at
(defun chop (list)
  (setf (cdr list) nil)
  list)

(defun ntack (symbol list)
  (setf (cdr (last list)) (list symbol))
  list)

;;; if nil is a concern
;;; it still cannot update a variable which value is nil
(defun ntack (symbol list)
  (cond ((null list) (list symbol))
        (t (setf (cdr (last list)) (list symbol))
           list)))

;;; haven't seen the abstraction
;;; fully recreated it above
(defun ntack (symbol list)
  (nconc list (list symbol)))

;;; or

(defun ntack (symbol list) ; this one fails
                           ; how to elegantly fix it?
  (nreverse (push symbol (nreverse list))))

;;; only works with version 1 of nreverse - nonportable
(defun ntack (symbol list)
  (setf list (nreverse list))
  (push symbol list)
  (nreverse list))

;;; only works wish version 2 of nreverse - nonportable
(defun ntack (symbol list)
  (nreverse list)
  (let ((rest (cdr list))
        (first (car list)))
    (setf (car list) symbol
          (cdr list) (cons first rest))
    (nreverse list)))

;;; works with any version - portable
(defun ntack (symbol list)
  (setf list (nreverse list))
  (let ((rest (cdr list))
        (first (car list)))
    (setf (car list) symbol
          (cdr list) (cons first rest))
    (nreverse list)))

(setf x '(a b c))

(setf (cdr (last x)) x)

;;; x
;;; |
;;; v
;;; |.|.| -> |.|.| -> |.|.| -> nil
;;;  |        |        |
;;;  v        v        v
;;;  a        b        c


;;; .--------------------.
;;; |                    |
;;; v                    |
;;; |.|.| -> |.|.| -> |.|.|
;;;  |        |        |
;;;  v        v        v
;;;  a        b        c

(setf h '(hi ho))

(append h h)
;;; the result is a 4 cons cell chain
;;; that shares 2 last cells with h
;;; (eq (cddr (append h h)) h) = t
;;; the first 2 cells are newly made
;;; and share their cars with h
;;; as follows
;;; (eq (append h h) h) = nil
;;; (eql (car (append h h)) (car h)) = t
;;; (eql (cadr (append h h)) (cadr h)) = t 
;;; eql is used in case of characters and nums
;;; every other object in car/cadr of a new chain
;;; will be eq to car/cadr of h


(nconc h h)
;;; just like 10.11, the result is a circular list
;;; this circular list is the list h points to
;;; with every cons cell from the original
;;; since h still points to the first cons cell
;;; and nconc changed the cdr of the last
;;; h now points to this circular list
;;;      .-----------.
;;;      |           |
;;;      v           |
;;; h -> |.|.| -> |.|.|
;;;       |        |
;;;       v        v
;;;       hi       ho

