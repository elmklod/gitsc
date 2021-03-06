(defun pilot-saying nil
  (format t 
   "~&There are old pilots,~%and there are bold pilots,~%but there are no old bold pilots."))

(defun draw-line (n)
  (labels ((rec (n) 
             (cond ((zerop n) (format t "~%"))
                   (t (format t "*") ; if we play dumb and don't know that cond can have many actions in clause, then just unite actions under or special function. It is known that every one of them returns nil, so every actions will be evaluated. this way it is only one action.
                      (rec (- n 1))))))
    (format t "~&")
    (rec n)))

(defun draw-box (width height) ; no ~& required because draw-line is using it already. If it wasn't known that it did, draw-box would have to use it too. Which allows draw-line not use it if draw-line is not to be used independently. Saves time.
  (cond ((zerop height) nil)
        (t (draw-line width)
           (draw-box width (- height 1)))))

(defun ninety-nine-bottles (n)
  (labels ((sing-verses (n)
             (cond ((zerop n)
                    (format t "Bartender! More beer!"))
                   (t (let ((bottles-or-bottle
                             (if (= n 1)
                                 "bottle"
                                 "bottles"))
                            (one-or-it
                             (if (= n 1)
                                 "it"
                                 "one"))
                            (next-n (- n 1)))
                        (format t
                         "~a ~a of beer on the wall,~%"
                         n bottles-or-bottle)
                        (format t
                         "~a ~a of beer!~%"
                         n bottles-or-bottle)
                        (format t
                         "Take ~a down,~%Pass it around,~%"
                         one-or-it)
                        (format t
                         "~a ~a of beer on the wall.~%~%"
                         next-n
                         (if (= 1 next-n)
                             "bottle"
                             "bottles"))
                        (sing-verses next-n))))))
    (format t "~&")
    (sing-verses n)))

(defun print-board (ttt-list)
  (if (not (= (length ttt-list) 9))
      (format t "~&No game for you.")
      (labels ((transform (symbol)
                 (if symbol
                     symbol
                     " "))
               (draw-separators nil
                 (format t "-----------~%"))
               (draw-line (list)
                 (format t
                  " ~a | ~a | ~a ~%"
                  (transform (first list))
                  (transform (second list))
                  (transform (third list))))
               (drawer (list)
                 (cond ((null list) nil)
                       (t (draw-line list)
                          (if (cdddr list)
                              (draw-separators))
                          (drawer (cdddr list))))))
        (format t "~&")
        (drawer ttt-list))))

(defun print-board (ttt-list)
  (if (not (= (length ttt-list) 9))
      (format t "~&No game for you.")
      (labels ((transform (symbol)
                 (if symbol
                     symbol
                     " "))
               (draw-separators nil
                 (format t "-----------~%"))
               (draw-line (list)
                 (format t
                  " ~a | ~a | ~a ~%"
                  (first list)
                  (second list)
                  (third list)))
               (drawer (list)
                 (cond ((null list) nil)
                       (t (draw-line list)
                          (if (cdddr list)
                              (draw-separators))
                          (drawer (cdddr list))))))
        (format t "~&")
        (drawer (mapcar #'transform 
                        ttt-list)))))