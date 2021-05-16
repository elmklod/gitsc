(defun my-count-up (n)
  (tr-count-up n nil))

(defun tr-count-up (n result)
  (cond ((zerop n) result)
        (t (tr-count-up (- n 1) (cons n result)))))

(defun my-fact (n)
  (tr-fact n 1))

(defun tr-fact (n result)
  (cond ((zerop n) result)
        (t (tr-fact (- n 1) (* n result)))))

(defun my-union (set1 set2)
  (tr-union set1 set2 nil))

(defun tr-union (set1 set2 set1-unique)
  (cond ((null set1) (append set1-unique set2)) ; depending on the average size of union and set2, their positions may be swapped, with the smaller taking the first, bigger the second
        ((member (first set1) set2)
         (tr-union (rest set1) set2 set1-unique))
        (t (tr-union 
            (rest set1)
            set2
            (cons (first set1)
                  set1-unique)))))

(defun my-union (set1 set2)
  (cond ((null set1) set2)
        ((member (first set1) set2)
         (my-union (rest set1) set2))
        (t (my-union
            (rest set1)
            (cons (first set1)
                  set2)))))

(defun my-intersection (set1 set2)
  (tr-intersection set1 set2 nil))

(defun tr-intersection (set1 set2 intersection)
  (cond ((null set1) intersection)
        ((member (first set1) set2)
         (tr-intersection
          (rest set1)
          set2
          (cons (first set1) intersection)))
        (t (tr-intersection
            (rest set1)
            set2
            intersection))))

(defun my-set-difference (set1 set2)
  (tr-set-difference set1 set2 nil))

(defun tr-set-difference (set1 set2 difference)
  (cond ((null set1) difference)
        ((member (first set1) set2)
         (tr-set-difference (rest set1) set2 difference))
        (t (tr-set-difference
            (rest set1)
            set2
            (cons (first set1)
                  difference)))))

;;; union can be concisely defined via set-difference
(defun my-union (set1 set2)
  (append (set-difference set1 set2)
          set2)) ; swap occurences of set1 and set2 and the result is still their union



