(defun my-member (element list) ; this version does (length list) conses. GC may be started
  (assoc element
    (reduce 
     (let ((n (length list)))
       (lambda (element table)
         (cons (nthcdr (setf n (- n 1))
                       list)
               table)))
     list
     :from-end t
     :initial-value nil)))
