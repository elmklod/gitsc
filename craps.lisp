(defun throw-die nil "Throws a die and returns the number on its upper side"
  (+ 1 (random 6)))

(defun throw-dice nil "Throws two dice and returns numbers on their upper sides in a list"
  (list (throw-die)
	(throw-die)))

(defun snake-eyes-p (throw)
  "Tests if the throw is snake-eyes(2 ones)"
  (equal throw '(1 1))) ; (= 2 (+ (car throw) (cadr throw)))

(defun boxcars-p (throw)
  "Tests if the throw is box-cars(2 6s)"
  (equal throw '(6 6)))

(defun throw-sum (throw)
  "Computes the sum of the throw by adding the results of 2 dice together"
  (+ (first throw)
     (second throw)))


(defun instant-win-p (throw)
  "Tests if the first throw is an instant win(sum is 7 or 11)"
  (let ((sum (throw-sum throw)))
    (or (= sum 7) (= sum 11))))

(defun instant-loss-p (throw)
  "Tests if the first throw is an instant loss(sum is 2, 12 or 3"
  (or (snake-eyes-p throw)
      (boxcars-p throw)
      (= 3 (throw-sum throw))))

(defun say-throw (throw)
  "Tells if the throw is snake-eyes or boxcars. If it's neither computes the throw sum"
  (cond ((snake-eyes-p throw)
	 'snake-eyes)
	((boxcars-p throw)
	 'boxcars)
	(t (throw-sum throw))))

(defun craps nil "Plays the first throw of craps"
  (let* ((throw (throw-dice))
	 (throw-1 (first throw))
	 (throw-2 (second throw))
	 (throw-is (say-throw throw)))
   (cond ((instant-loss-p throw)      ; could do away with conses, but the code would grow in size and become quite unpleasant to look at
	  (list 'throw throw-1 'and throw-2
		'-- throw-is '-- 'you 'lose))
	 ((instant-win-p throw)
	  (list 'throw throw-1 'and throw-2
		'-- throw-is '-- 'you 'win))
	 (t
	  (list 'throw throw-1 'and throw-2
	        '-- 'your 'point 'is throw-is)))))

(defun try-for-point (point)
  "Throws the dice, if the new point is 7 you lose, if it equals the original point you win, otherwise you have to throw again with the point parameter being the original point"
  (let* ((throw (throw-dice))
	 (throw-1 (car throw))
	 (throw-2 (cadr throw))
	 (sum (throw-sum throw))) ; It is unclear if the program should mention boxcars and snake-eyes specifically, they do not matter, except that they certainly, along with sum = 3, make you throw again
   (cond ((= point sum)
	  (list 'throw throw-1 'and throw-2
		'-- sum '-- 'you 'win))
	 ((= sum 7)
	  (list 'throw throw-1 'and throw-2
		'-- sum '-- 'you 'lose))
	 (t
	  (list 'throw throw-1 'and throw-2
	        '-- sum '-- 'throw 'again)))))
