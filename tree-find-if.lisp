(defun tree-find-if (pred tree)
  (cond ((null tree) nil)
        ((atom tree) (and (funcall pred tree)
                          tree))
        (t (or (tree-find-if pred (car tree))
               (tree-find-if pred (cdr tree))))))