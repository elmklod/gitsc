;;; symbols are blocks of 5 pointers:
;;; name(a string), package, property-list, value, function
;;; each pointer can be extracted with the apropriate selector
;;; applied to a symbol
;;; A result of a symbol evaluation depends on its context
;;; and may(possibly) be one of the five pointers or something else
;;; associated with any one of the pointers
;;; Their associated name string is always upper-care

;;; strings are sequences(vectors) or zero or more characters.
;;; They are delimited by "" and always evaluate to themselves
;;; can contain both upper and lower case characters

(format t "a~S" 'b)
;;;aB
;;;NIL

(format t "always~%broke")
;;;always
;;;broke
;;;NIL

(format t "~S~S" 'alpha 'bet)
;;;ALPHABET
;;;NIL
