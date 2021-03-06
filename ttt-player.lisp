(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun print-board (ttt-list)
  (if (not (= (length ttt-list) 10))
      (format t "~&No game for you.")
      (labels ((transform (n)
                 (cond ((= n 0) " ")
                       ((= n 1) "O")
                       ((= n 10) "X")))
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
                        (rest ttt-list))))))

(setf *computer* 10)
(setf *opponent* 1)

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9) ;H
        (1 4 7) (2 5 8) (3 6 9) ;V
        (1 5 9) (3 5 7)))       ;D

(defun sum-triplet (board triplet)
  (reduce (lambda (sum pos) (+ sum
                               (nth pos board)))
          triplet
          :initial-value 0))

(defun compute-sums (board)
  (mapcar (lambda (triplet)
            (sum-triplet board triplet))
          *triplets*))

(defun winner-p (board)
  (find-if (lambda (n) (or (= n (* 3 *computer*))
                           (= n (* 3 *opponent*))))
           (compute-sums board)))

(defun play-one-game ()
  (let ((new-board (make-board)))
    (if (y-or-n-p "~&Would you like to go first? ")
        (opponent-move new-board)
        (computer-move new-board))))

(defun opponent-move (board)
  (let* ((move (read-a-legal-move board))
         (played-board (make-move *opponent* move board)))
    (print-board played-board)
    (cond ((winner-p played-board)
           (format t "~&You win!"))
          ((board-full-p played-board)
           (format t "~&It's a tie!"))
          (t (computer-move played-board)))))


(defun board-not-full-p (board)
  (find-if (lambda (mark) ; ok, member solution is actually smaller and cleaner
             (zerop mark))
           (rest board)))

(defun my-board-full-p (board)
  (not (board-not-full-p board)))

(defun board-full-p (board)
  (not (member 0 (rest board))))

(defun read-a-legal-move (board)
  (let ((move (read)))
    (cond ((not (and (integerp move)
                     (<= 1 move 9)))
           (format t "~&Invalid input.~%")
           (read-a-legal-move board))
          ((not (zerop (nth move board)))
           (format t "~&That space is already occupied.~%")
           (read-a-legal-move board))
          (t move))))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (move (first best-move))
         (strategy (second best-move))
         (played-board (make-move *computer* move board)))
    (format t "~&My move: ~s~%My strategy: ~a" move strategy)
    (print-board played-board)
    (cond ((winner-p played-board)
           (format t "~&I win!"))
          ((board-full-p played-board)
           (format t "~&It's a tie!"))
          (t (opponent-move played-board)))))

(defun choose-best-move (board)
  (random-move-strategy board))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
        (pick-random-empty-position board))))
  
(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board
                           (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board
                           (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if
                  (lambda (trip)
                    (= (sum-triplet board trip)
                       target-sum))
                  *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if (lambda (pos)
             (zerop (nth pos board)))
           squares))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (set-squeeze-play board)
      (set-2-on-1 board)
      (exploit-2-on-1 board)
      (random-move-strategy board)))

(setf *corners* '(1 3 7 9))
(setf *sides* '(2 4 6 8))
(setf *diagonals* '((1 5 9) (3 5 7)))
(setf *center* 5)
(setf *empty* 0)

(defun search-diagonals (sum board)
  (find-if (lambda (diag)
             (= sum (sum-triplet board diag)))
           *diagonals*))

(defmacro if-found-return-pred (pred &rest r)
  (let ((block-name (gensym)))
    `(block ,block-name
       ,(cons
         'find-if
         (cons
          `(lambda (x)
             (let ((p-val (funcall ,pred x)))
               (if p-val
                   (return-from ,block-name p-val))))
          r)))))
              
(defun find-and-solve-diag-pattern
       (d-sum expected-center pos-fn strategy board)
  (and (= (nth *center* board) expected-center)
       (let ((pos
              (if-found-return-pred
               (lambda (diag)
                 (and (= (sum-triplet board diag) d-sum)
                      (funcall pos-fn diag)))
               *diagonals*)))
         (when pos
           (list pos strategy)))))

(defun block-squeeze-play (board)
  (find-and-solve-diag-pattern
   12
   *computer*
   (lambda (diag) (find-empty-position board *sides*))
   "block squeeze play"
   board))

(defun block-2-on-1 (board)
  (find-and-solve-diag-pattern
   12
   *opponent*
   (lambda (diag) (find-empty-position board *corners*))
   "block two on one"
   board))

(defun set-squeeze-play (board)
  (find-and-solve-diag-pattern
   11
   *opponent*
   (lambda (diag) (find-empty-position board diag))
   "set squeeze play"
   board))

(defun set-2-on-1 (board)
  (if-found-return-pred
   (lambda (plausible-center-val)
     (find-and-solve-diag-pattern
      11
      plausible-center-val
      (lambda (diag) (find-empty-position board diag))
      "set two on one"
      board))
   (list *empty* *computer*)))

(defun exploit-2-on-1 (board)
  (find-and-solve-diag-pattern
   21
   *computer*
   (lambda (diag)
     (let* ((r-other-diag (remove *center*
                                  (find-if-not
                                   (lambda (d) (equal d diag))
                                   *diagonals*)))
            (r-diag (remove *center* diag))
            (comp-pos (find-if
                       (lambda (p) (= (nth p board)
                                      *computer*))
                       r-diag))
            (opp-pos (find-if
                      (lambda (p) (/= p comp-pos))
                      r-diag))
            (corner-poses (mapcar (lambda (p)
                                    (list
                                     p
                                     (/ (+ p comp-pos)
                                        2)
                                     (find-if-not
                                      (lambda (other-p)
                                        (= other-p p))
                                      r-other-diag)))
                                  r-other-diag))         
            (side-poses (mapcar (lambda (p)
                                  (list (/ (+ comp-pos p) 2)
                                        p
                                        (/ (+ (find-if-not
                                               (lambda (other-p)
                                                 (= other-p p))
                                               r-other-diag)
                                              opp-pos)
                                           2)))
                                r-other-diag)))
       (if-found-return-pred (lambda (list)
                               (and (every (lambda (p)
                                             (zerop (nth p board)))
                                           list)
                                    (first list)))
                             (nconc side-poses corner-poses))))
                         
   "exploit two on one"
   board))
       
