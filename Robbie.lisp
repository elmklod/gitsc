(setf rooms
      '((library (east upstairs-bedroom)
	   	 (south back-stairs))
        (upstairs-bedroom (west library)
		 	  (south front-stairs))
        (front-stairs (north upstairs-bedroom)
 		      (south living-room))
        (living-room (north front-stairs)
	 	     (east kitchen)
		     (south dining-room))
        (kitchen (west living-room)
	 	 (south pantry))
        (pantry (north kitchen)
	        (west dining-room))
        (dining-room (north living-room)
 	      	      (east pantry)
		      (west downstairs-bedroom))
        (downstairs-bedroom (east dining-room)
			    (north back-stairs))
        (back-stairs (south downstairs-bedroom)
	 	     (north library))))

(defun choices (room) "Returns the table of permissible directions Robbie may take from a given room"
 (rest (assoc room rooms)))

(defun look (direction room) "Tells where Robbie would end up if he moved in a given direction from a given room"
 (second (assoc direction (choices room))))

(setf LOC 'pantry)

(defun set-robbie-location (place)
 "Moves Robbie to PLACE by setting
  the variable LOC"
  (setf loc place))

(defun how-many-choices ()
 "Tells how many choices Robbie has for where to move to next"
 (length (choices loc)))

(defun upstairsp (room)
 "Checks if a room is upstairs(the library or upstairs-bedroom)"
 (or (equal room 'upstairs-bedroom) (equal room 'library))) ; (require (equal room (amb 'library 'upstairs-bedroom)))

(defun onstairsp (room)
 "Checks if a room is stairs(back-stairs or front-stairs)"
 (or (equal room 'back-stairs)
     (equal room 'front-stairs)))

(defun where () "Tells where Robbie is"
 (append '(Robbie is)
         (cond ((onstairsp loc) '(on))
               ((upstairsp loc) '(upstairs in))
	       (t '(downstairs in)))
         (list 'the loc))) ; substitute loc with (if (bedroomp loc)
                           ;                         'bedroom
                           ;                         loc)

(defun bedroomp (room) "Checks if the room is a bedroom(upstairs-bedroom or downstairs-bedroom)"
 (or (equal room 'downstairs-bedroom)
     (equal room 'upstairs-bedroom)))

(defun move (direction) "Moves Robbie in a given direction. If there is no path, Robbie hits a wall and stays at his original location"
 (let ((next-loc (look direction loc)))
  (cond (next-loc (set-robbie-location next-loc)
	          (where))
	(t '(Ouch! Robbie hit a wall)))))
