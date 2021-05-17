;;; an important topic is whether output produced by macro calls
;;; should be copied or not by copy-tree
;;; Copying it allows for the produced code to be mutated.
;;; What possibilities this opens is limited only by one's own imagination
;;; One way is to include copying in the defmacro, if it is not
;;; already.
;;; The other way is to provide some flag which would
;;; copy the produced code if t and wouldn't if nil.
;;; otherwise the user is resposible for the copying
;;; by explicitly inserting calls to copy-tree when such copying
;;; is desired.
;;; in case of backquote this is already a built-in behaviour since it recreates every list anew
;;; Need to confirm it further.

(defmacro if-found-return-pred (pred &rest r)
  (let ((block-name (gensym)))
    `(block ,block-name
       ,(cons
         'find-if
         (cons
          `(lambda (x)
             (let ((p-val (funcall ,pred x)))
               (if p-val
                   (return-from ,block-name p-val))))
          r)))))
;;; very simple recursive definition
(defun list-conses (list)
  (cond ((null list) nil)
        (t (cons list (list-conses (rest list))))))

(defun list-conses (list)
  (if list
      (cons list (list-conses (rest list)))))

(defun list-conses (list)
  (labels ((rec (list L)
             (cond ((null list) (nreverse L))
                   (t (rec (rest list)
                           (cons list L))))))
    (rec list nil)))

(defun list-conses (list)
  (labels ((rec (list L)
             (if list
                 (rec (rest list)
                      (cons list L))
                 (nreverse L))))
    (rec list nil)))

;;; naive, can't handle circular lists
(defun list-conses (list)
 (do ((L nil))
     ((null list) (nreverse L))
   (push list L)
   (pop list)))

;;; Had I found it earlier, the macro and the above 
;;; fun def would not have come into existence.
(defun list-conses (list)
  (maplist 'identity list)) ; or #'

;;; It is the exact equivalent of maplist.
(defmacro mapcons (f &rest lists)
  `(mapcar ,f ,@(mapcar (lambda (list)
                          (list 'list-conses list))
                        lists)))

;;; Mapcar functionality can be implemented via maplist/mapcons

(defmacro mapcar (f &rest lists)
  `(maplist ,(let ((params nil))
               (dolist (x lists params)
                 (push (gensym) params))
               `(lambda ,params
                  (apply ,f 
                         ,(cons 'list
                                (maplist (lambda (cell)
                                           (list
                                            'car
                                            (car cell)))
                                         params)))))
            ,@lists))
;;; equivalent and a little shorter
(defmacro mapcar (f &rest lists)
  `(maplist ,(let ((params nil))
               (dolist (x lists params)
                 (push (gensym) params))
               `(lambda ,params
                  (funcall ,f 
                         ,@(maplist (lambda (cell)
                                      (list
                                       'car
                                       (car cell)))
                                    params))))
            ,@lists))
