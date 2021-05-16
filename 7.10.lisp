(defun all-odd (numbers)
 (every #'oddp numbers))

(defun none-odd (numbers)
 (every #'evenp numbers))

(defun not-all-odd (numbers)
 (not (all-odd numbers))) ; (find-if #'evenp numbers) faster

(defun not-none-odd (numbers)
 (not (none-odd numbers))) ; (find-if #'oddp numbers)


