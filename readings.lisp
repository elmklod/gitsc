(defun compute-pay nil
  (let* ((hwage (or
                 (format t ; let* to guarantee the sequential nature of assignment
                  "~&What is worker's hourly wage?: ")
                 (read)))
         (hours (or 
                 (format t
                  "~&How many work-hours has the worker?: ")
                 (read))))
    (format t 
     "~&The worker's gross pay is ~s~%"
     (* hwage hours))))

(defun cookie-monster ()
  (format t "~&Give me cookie!!!!~%")
  (format t "Cookie? ")
  (let ((item (read)))
    (if (eq item 'cookie)
        (format t
         "~&Thank you!...Munch munch munch... BURP~%") ; can do without ~& in the beginning as it will always put a new line when cookie is read. If this response is implementation-dependent, put ~&
        (or (format t
             "~&No want ~s...~%~%" item)
            (cookie-monster)))))

