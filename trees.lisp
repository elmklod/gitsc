(defun arith-eval (a-exp)
  (cond ((numberp a-exp) a-exp)
        (t (funcall
            (symbol-function (second a-exp)) ; outsmarted, just (second a-exp) will do, even better, the semantics are exactly equivalent as the function will be extracted from the global environment, which is what this code does explicitly 
            (arith-eval (first a-exp))
            (arith-eval (third a-exp))))))

(defun legalp (expression)
  (or (numberp expression)
      (and (consp expression)
           (= 3 (length expression))
           (member (second expression)
                   '(+ - * /))
           (legalp (first expression))
           (legalp (third expression)))))

;;; nil is a proper list, and so is any cons cell whose cdr is a proper list

;;; A positive integer greater than 1 is either a prime number or 
;;; a product of a prime number and a positive integer greater than 1,
;;; both are smaller than the original number. <-- excessive

(defun factors (number)
  (labels ((rec (possible-factor)
             (cond ((> possible-factor
                       (sqrt number))
                    (list number))
                   ((zerop (rem number possible-factor)) ; there is no need to check that possible-factor has only one factor(itself), because this function extracts factors starting from the smallest and the first smallest positive integers > 1 are prime (2, 3), which means that their composites are not possible in the list, then only the next prime is allowed in the list and the composites of it and previous primes are impossible in the list and so forth.
                    (cons possible-factor
                          (factors (/ number possible-factor))))
                   (t (rec (+ 1 possible-factor))))))
    (rec 2)))
;;; the book version is more efficient as it does not restart the
;;; factorization of a quotient number bruteforcing from 2. Instead it
;;; resumes from the factor already known to be the smallest factor
;;; in the previous iteration number.

(defun factor-tree (number)
  (labels ((rec (possible-factor)
             (cond ((> possible-factor
                       (sqrt number))
                    number)
                   ((zerop (rem number possible-factor)) 
                    (list
                     number
                     possible-factor
                     (factor-tree (/ number possible-factor))))
                   (t (rec (+ 1 possible-factor))))))
    (rec 2)))

(defun book-factors (n)
  (book-factors-help n 2))

(defun book-factors-help (n p)
  (cond ((equal n 1) nil)
        ((zerop (rem n p))
         (cons p (book-factors-help (/ n p) p)))
        (t (book-factors-help n (+ p 1)))))

(defun book-tree-factors (n)
  (book-tree-factors-help n 2))

(defun book-tree-factors-help (n p)
  (cond ((= n p) p)
        ((zerop (rem n p))
         (list n p (book-tree-factors-help (/ n p) p)))
        (t (book-tree-factors-help n (+ p 1)))))

;;;   .
;;;  / \
;;; A   .
;;;    / \
;;;   B   .
;;;      / \      increased branches length to avoid the seeming node
;;;     /   \     collision, it has no meaning other than to draw 
;;;    .     .    concisely
;;;   / \   / \
;;;  C   . E  nil
;;;     / \
;;;    D  nil


;;; Classification of major modern lisp dialects
;;;
;;;                  Lisp
;;;                 /    \
;;;            Lisp-n    Lisp-1______
;;;           /  \        /   |     \    
;;;    ______CL  Shen  Racket Scheme Clojure
;;;   / |  |...\    |  |    |  |...|  |    \
;;; LW ACL SBCL CCl Kl BC   CS Chez|  JVM  JS
;;;  |  |  |    |   |  |    |  |  MIT |     |
;;;  |  |  |   /    |  |    |  |   |  |     |
;;;  $  $  F   F  F->$ F    F  F   F  F     F
;;;  I  I  I   I    I  D    I  I   S  I     I
