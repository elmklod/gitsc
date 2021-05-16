(defun fib (n)
  (cond ((or (= n 1)
             (= n 0))
         1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))
;;; 8.12
;;; '( 1 2 3 5 6 7 8 7) - works
;;; () - recurses infinitely

;;; 8.13
;;; any negative number or any positive non-integer numbers

(defun the-shortest-infinite-recursion-function ()
 (the-shortest-infinite-recursion-function))

;;; let l = #1=(X . #1#)
;;; (car l) = X
;;; (cdr l) = l (#1=(X . #1#))
;;; count-slices will recurse infinitely, constantly adding one
;;; to the result of (count-slices l)
;;; l stands here not as a lisp symbol, but as a variable to denote an object.
;;; There is some other symbol in its place that denotes a lisp variable assigned to l(or l assigned to it, is it reflexive? It is)
