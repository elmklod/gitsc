(defun right-side (list) "Returns the right side object description"
  (rest (member '-vs- list)))

(defun left-side (list) "Returns the left side object description"
  (rest (member '-vs- (reverse list)))) ; or (right-side (reverse list))

;;; (defun left-side (list)
;;;  (remove '-vs- (set-difference list (right-side list))))

(defun count-common (list)
  "Returns the number of features the left and right objects have in common"
  (length (intersection (left-side list)
			(right-side list))))

(defun compare (list)
  "Reports the number of features the two objects have in common"
  (list (count-common list) ;or (cons (count-common list) '(common features))
	'common
	'features))
