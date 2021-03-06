(defun count-up (n)
  (cond ((zerop n) nil)
        (t (append (count-up (- n 1))
                   (list n)))))

(defun make-loaf (n)
  (if (not (zerop n))
      (cons 'x (make-loaf (- n 1)))))

(defun make-loaf (n)
  (if (zerop n)
      nil
      (cons 'x (make-loaf (- n 1)))))

;;; single-test augmenting recursion
(defun bury (item n)
  (cond ((zerop n)
         item)
        (t (list (bury item (- n 1))))))

(defun pairings (list1 list2)
  (cond ((null list1)
         list2)
        ((null list2)
         list1)
        (t (cons (list (first list1)
                       (first list2))
                 (pairings (rest list1)
                           (rest list2))))))

(defun sublists (list)
  (if list
      (cons list (sublists (rest list)))))

;;; () is also a sublist
(defun sublists (list)
  (if list
      (cons list (sublists (rest list)))
      (list nil)))

;;; still simple
(defun my-reverse (list)
  (if list
      (append (my-reverse (rest list))
              (list (first list)))))

(defun my-reverse (list)
  (my-reverse-rec list ()))

(defun my-reverse-rec (list result)
  (if list
      (my-reverse-rec
       (rest list)
       (cons (first list)
             result))
      result))

;;; experimental
(defun my-union (set1 set2)
  (cond ((null set1)
         set2)
        (t (let ((subunion 
                  (my-union
                   (rest set1)
                   set2)))
             (if (member (first set1)
                         set2)
                 subunion
                 (cons (first set1)
                       subunion))))))

;;; no helping function necessary
(defun my-union (set1 set2)
  (cond ((null set1)
         set2)
        ((member (first set1)
                 set2)
         (my-union (rest set1)
                   set2))
        (t (cons (first set1)
                 (my-union (rest set1)
                           set2)))))

(defun largest-even (numlist)
  (if numlist
      (max (let ((n (first numlist)))
             (if (evenp n)
                 n
                 0))
           (largest-even (rest numlist)))
      0))

;;; More clear alternative
(defun largest-even (numlist)
  (cond ((null numlist) 0)
        ((evenp (first numlist))
         (max (first numlist)
              (largest-even (rest numlist))))
        (t (largest-even (rest numlist)))))

(defun largest-even (numlist)
  (largest-even-rec numlist 0))

;;; Tail-recursive
(defun largest-even-rec (numlist number)
  (cond ((null numlist)
         number)
        (t (largest-even-rec
            (rest numlist)
            (max number
                 (let ((n (first numlist)))
                   (if (evenp n)
                       n
                       0)))))))
;;; Alternative          
(defun largest-even-rec (numlist number)
  (cond ((null numlist)
         number)
        (t (largest-even-rec
            (rest numlist)
            (if (evenp (first numlist))
                (max number
                     (first numlist))
                number)))))       

(defun huge (n)
  (huge-rec n n))

(defun huge-rec (n exponent)
  (if (zerop exponent)
      1
      (* n (huge-rec n (- exponent 1)))))

(defun huge (n)
  (huge-rec n n 1))

(defun huge-rec (n exponent result)
  (if (zerop exponent)
      result
      (huge-rec 
       n
       (- exponent 1)
       (* result n))))


;;; I take no orders
(defun huge (n)
  (huge-rec n n))

(defun huge-rec (n exponent)
  (if (zerop exponent)
      1
      (reduce #'*  (cons n
                         (list
                          (huge-rec
                           n
                           (- exponent 1)))))))
