(defun find-first-odd (list)
  (cond ((null list)
         nil)
        ((oddp (first list))
         (first list))
        (t (find-first-odd (rest list)))))

(defun last-element (list)
  (cond ((atom (cdr list))
         (car list))
        (t (last-element (cdr list)))))

;;; Augmenting
(defun fact (n)
 (cond ((= n 0) 1)
       (t (* n (fact (- n 1))))))

(defun add-nums (n)
  (cond ((= n 0) 0)
        (t (+ n (add-nums (- n 1))))))

(defun all-equal (list)
  (cond ((null list)
         t)
        (t (and (equal (first list)
                       (second list))
                (all-equal (rest list))))))

(defun all-equal (list)
  (cond ((null list) t)
        ((not (equal (first list)
                     (second list)))
         nil)
        (t (all-equal (rest list)))))

;;; 5    ha    (ha ha ha ha) (ha ha ha ha ha)

;;; 4    ha    (ha ha ha) (ha ha ha ha)

;;; 3    ha    (ha ha)  (ha ha ha)

;;; 2    ha    (ha) (ha ha)

;;; 1    ha    nil  (ha)

;;; 0     returns nil

(defun count-down (n)
  (cond ((zerop n) nil)
        (t (cons n ;or (> n 0) to return nil for negative inputs
                 (count-down (- n 1))))))

(defun applicative-fact (n)
  (reduce #'* (count-down n)))

(defun count-down-mod1 (n)
  (append (count-down n) (list 0)))


(defun count-down-mod2 (n)
  (cond ((zerop n) (list 0))
        (t (cons n ;or (> n 0) to return nil for negative inputs
                 (count-down (- n 1))))))

(defun square (n) (* n n))

(defun square-list (list)
  (cond ((null list) nil)
        (t (cons (square
                  (first list))
                 (square-list (rest list))))))

(defun my-nth (n list)
  (cond ((null list) nil) ; first and second clauses may be interchanged
        ((zerop n) (first list))
        (t (my-nth (- n 1) (rest list)))))


(defun my-nth (n list)
  (cond ((or (zerop n) ; also valid
             (null list))
         (first list))
        (t (my-nth (- n 1) (rest list)))))

(defun my-member (element list)
  (cond ((null list)
         nil)
        ((eql element (first list))
         list)
        (t (my-member element (rest list)))))

(defun my-assoc (key table)
  (cond ((null table) nil)
        ((eql key (caar table)) ;or (car (first table))
         (first table))
        (t (my-assoc key (rest table)))))

(defun compare-lengths (list1 list2)
  (cond ((and (null list1)
              (null list2))
         'same-length)
        ((null list1)
         'second-is-longer)
        ((null list2)
         'first-is-longer)
        (t (compare-lengths (rest list1)
                            (rest list2)))))

(defun sum-numeric-elements (list)
  (cond ((null list) 0)
        ((numberp (first list))
         (+ (first list)
            (sum-numeric-elements (rest list))))
        (t (sum-numeric-elements (rest list)))))

(defun my-remove (element list)
  (cond ((null list) nil)
        ((eql element (first list))
         (my-remove element (rest list)))
        (t (cons (first list)
                 (my-remove element
                            (rest list))))))

;;; (rest set1) and set2 can be swapped
;;; not clear if it brings improvements
(defun my-intersection (set1 set2)
  (cond ((or (null set1)
             (null set2))
         nil)
        ((my-member (first set1)
                    set2)
         (cons (first set1)
               (my-intersection (rest set1)
                                set2)))
        (t (my-intersection (rest set1)
                            set2))))

(defun my-set-difference (set1 set2)
  (cond ((null set2) ; excessive on every recursive call except the first one
         set1)
        ((null set1)
         nil)
        ((my-member (first set1)
                    set2)
         (my-set-difference (rest set1)
                            set2))
        (t (cons (first set1)
                 (my-set-difference
                  (rest set1)
                  set2)))))

;;; alternative version
(defun my-set-difference (set1 set2)
  (cond ((null set2)
         set1)
        ((null set1) ; excessive on every recursive call except the first one
         nil)
        ((my-member (first set2)
                    set1)
         (my-set-difference (my-remove 
                             (first set2)
                             set1)
                            (rest set2)))
        (t (my-set-difference
            set1
            (rest set2)))))


(defun count-odd (numlist)
  (cond ((null numlist)
         0)
        ((oddp (first numlist))
         (+ 1 (count-odd (rest numlist))))
        (t (count-odd (rest numlist)))))

(defun count-odd (numlist)
  (cond ((null numlist)
         0)
        (t (+ (if (oddp (first numlist))
                  1
                  0)
              (count-odd (rest numlist))))))
              
(defun combine (x y)
  (+ x y))

(defun fib (n)
  (cond ((= n 0)
         0)
        ((= n 1)
         1)
        (t (combine (fib (- n 1))
                    (fib (- n 2))))))
;;; only nonterminal calls call combine
;;; while the terminal do not.

;;; 8.38
;;; every nil would be replaced with q
;;; including implicit nils used to end lists

(defun count-atoms (tree)
  (cond ((atom tree) 1)
        (t (+ (count-atoms (car tree))
              (count-atoms (cdr tree))))))

(defun count-cons (tree)
  (cond ((consp tree)
         (+ 1
            (count-cons (car tree))
            (count-cons (cdr tree))))
        (t 0)))

(defun sum-tree (tree)
  (cond ((numberp tree)
         tree)
        ((atom tree)
         0)
        (t (+ (sum-tree (car tree))
              (sum-tree (cdr tree))))))

(defun my-subset (set1 set2)
  (cond ((null set1)
         t)
        (t (and (my-member (first set1)
                           set2)
                (my-subset (cdr set1)
                           set2)))))

(defun my-subst (new old tree)
  (cond ((eql old tree)
         new)
        ((atom tree)
         tree)
        (t (cons (my-subst
                  new
                  old
                  (car tree))
                 (my-subst
                  new
                  old
                  (cdr tree))))))

;;; The book version produces invalid result for ()
(defun flatten (tree)
  (cond ((null tree)
         nil)
        ((atom tree)
         (list tree))
        (t (append (let ((tree-car (car tree)))
                     (if tree-car
                         (flatten tree-car)
                         (list nil)))
                   (flatten (cdr tree))))))

(defun tree-depth (tree)
  (cond ((atom tree) 0)
        (t (let ((car-max-depth
                  (tree-depth (car tree)))
                 (cdr-max-depth
                  (tree-depth (cdr tree))))
             (+ 1 (if (> car-max-depth
                         cdr-max-depth)
                      car-max-depth
                      cdr-max-depth))))))
;;; uses max
(defun tree-depth (tree)
  (cond ((atom tree) 0)
        (t (+ 1 (max
                 (tree-depth (car tree))
                 (tree-depth (cdr tree)))))))

;;; This version views nil as ()
(defun paren-depth (tree)
  (cond ((null tree) 1)
        ((atom tree) 0)
        (t (let ((car-max-pars
                  (+ 1
                     (paren-depth
                      (car tree))))
                 (cdr-max-pars
                  (paren-depth
                   (cdr tree))))
             (if (> car-max-pars
                    cdr-max-pars)
                 car-max-pars
                 cdr-max-pars)))))

;;; uses max
(defun paren-depth (tree)
  (cond ((null tree) 1)
        ((atom tree) 0)
        (t (max
            (+ 1
               (paren-depth
                (car tree)))
            (paren-depth
             (cdr tree))))))

;;; This version views nil as an atom
(defun paren-depth (tree)
  (cond ((atom tree) 0)
        (t (let ((car-max-pars
                  (+ 1
                     (paren-depth
                      (car tree))))
                 (cdr-max-pars
                  (paren-depth
                   (cdr tree))))
             (if (> car-max-pars
                    cdr-max-pars)
                 car-max-pars
                 cdr-max-pars)))))
;;;
(defun paren-depth (tree)
  (cond ((atom tree) 0)
        (t (max
            (+ 1
               (paren-depth
                (car tree)))
            (paren-depth
             (cdr tree))))))
