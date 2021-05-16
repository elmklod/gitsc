(defun rank (card) "Returns the rank of a card"
 (first card))

(defun suit (card) "Returns the suit of a card"
 (second card))

(setf my-hand '((3 hearts)
		(5 clubs)
		(2 diamonds)
		(4 diamonds)
		(ace spades)))

(defun count-suit (suit hand) 
 "Returns the number of cards in a hand belonging to the suit"
 (length (remove-if-not (lambda (card)
		         (eq suit (suit card)))
         	        hand)))

(setf colors '((clubs black)
	       (diamonds red)
	       (hearts red)
	       (spades black)))

(defun color-of (card) "Tells the color of a card"
 (second (assoc (suit card) colors)))

(defun first-red (hand) 
 "Returns the first card of hand that is of a red suit, nil if there is none"
 (find-if (lambda (card)
	   (eq (color-of card)
	       'red))
	  hand))

(defun black-cards (hand) 
 "Returns a list of all the black cards in a hand"
 (remove-if-not (lambda (card)
	         (eq (color-of card)
         	     'black))
	        hand))

(defun what-ranks (suit hand)
 "Returns the ranks of all cards in a hand belonging to the suit"
 (mapcar ; apply remove-duplicates if a set is needed instead of a list with possible duplicates
  #'rank
  (remove-if-not (lambda (card)
                  (eq (suit card)
        	      suit))
        	 hand)))

(setf all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (card1 card2)
 "Tells if the rank of the first card is higher than the second"
 (member (rank card1) (rest (member (rank card2) all-ranks))))

(defun high-card (hand)
 "Returns the highest ranked card in a hand"
 (assoc (find-if (lambda (rank)
	          (assoc rank hand))
	 (reverse all-ranks))
	hand))

(defun high-card (hand)
 (labels ((recur (ranks)
           (let ((card (assoc (first ranks) hand)))
	    (if card ; (not (null card))
	        card
		(recur (rest ranks))))))
  (recur (reverse all-ranks))))

(defun high-card (hand)
 (reduce (lambda (card1 card2)
	  (if (higher-rank card1 card2)
	      card1
	      card2))
	 hand))
