(defun add-up (numlist)
  (cond ((null numlist) 0)
        (t (+ (first numlist) 
              (ADD-UP (REST NUMLIST))))))

(DEFUN ALLODDP (NUMLIST)
  (COND ((NULL NUMLIST) T)
        (T (AND (ODDP (FIRST NUMLIST))
                (alloddp (rest numlist))))))

(defun rec-member (element list)
  (cond ((null list) nil)
        ((eql element
              (first list))
         list)
        (t (rec-member element (rest list)))))

(defun rec-assoc (key table)
  (cond ((null table)
         nil)
        ((eql key 
              (car
               (first table))) ; optimize the accessing via let-bindings
         (first table))
        (t (rec-assoc key (rest table)))))


(defun rec-nth (n list)
  (cond ((zerop n)
         (first list))
        (t (rec-nth (- n 1) (rest list)))))

(defun add1 (n) (+ n 1))

(defun sub1 (n) (- n 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (sub1 y)))))

(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (add1 (rec-plus x (sub1 y))))))
