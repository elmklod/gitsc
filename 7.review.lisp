(defun my-find-if (pred list)
 (first (remove-if-not pred list)))

(defun my-every (pred list)
 (null (remove-if pred list)))
