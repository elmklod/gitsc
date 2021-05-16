(setf words
      '((one un)
	(two deux)
	(three trois)
	(four quatre)
	(five cinq)))


(setf trilingual-dict 
 (mapcar (lambda (entry spanish-word)
          (append entry 
	          (list spanish-word)))
         words 
  	 '(uno dos tres quatro cinco)))
