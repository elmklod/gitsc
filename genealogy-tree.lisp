(setf family '((colin nil nil)
               (deirdre nil nil)
               (arthur nil nil)
               (kate nil nil)
               (frank nil nil)
               (linda nil nil)
               (suzanne colin deirdre)
               (bruce arthur kate)
               (charles arthur kate)
               (david arthur kate)
               (ellen arthur kate)
               (george frank linda)
               (hillary frank linda)
               (andre nil nil)
               (tamara bruce suzanne)
               (vincent bruce suzanne)
               (wanda nil nil)
               (ivan george ellen)
               (julie george ellen)
               (marie george ellen)
               (nigel andre hillary)
               (frederick nil tamara)
               (zelda vincent wanda)
               (joshua ivan wanda)
               (quentin nil nil)
               (robert quentin julie)
               (olivia nigel marie)
               (peter nigel marie)
               (erica nil nil)
               (yvette robert zelda)
               (diane peter erica)))

(defun father (person)
  (second (assoc person family)))

(defun mother (person)
  (third (assoc person family)))

(defun parents (person) ; even memoized assoc could not compensate for using father and mother functions if it memoizes all possible people in a family
  (let* ((parents (rest (assoc person family)))
         (father (first parents))
         (mother (second parents)))
    (append (if father              ; if it is possible to have a person whose father and mother are the same person, use my-union instead of append
                (list father))
            (if mother
                (list mother)))))


(defun children (person)
  (and person 
       (my-mapcar #'first 
                  (my-remove-if-not
                   (lambda (entry)
                     (member person (rest entry)))
                   family))))

(defun my-mapcar (function list)
  (cond ((null list) nil)
        (t (cons (funcall function (first list))
                 (my-mapcar function (rest list))))))

(defun my-remove-if-not (pred list)
  (cond ((null list) nil)
        ((funcall pred (first list))
         (cons (first list)
               (my-remove-if-not
                pred
                (rest list))))
        (t (my-remove-if-not
            pred
            (rest list)))))

(defun children-2 (person)
  (and person (children-2-rec person family)))

(defun children-2-rec (person famtree)
  (cond ((null famtree) nil)
        ((member person (rest (first famtree)))
         (cons (first (first famtree))
               (children-2-rec person
                               (rest famtree))))
        (t (children-2-rec person
                           (rest famtree)))))

(defun my-union (set1 set2)
  (cond ((null set1)
         set2)
        ((member (first set1)
                 set2)
         (my-union (rest set1)
                   set2))
        (t (cons (first set1)
                 (my-union (rest set1)
                           set2)))))


(defun siblings (person)
  (my-set-remove        ; set-difference would also suffice
   person
   (my-union (children (father person))
             (children (mother person)))))

(defun siblings-2 (person)
  (my-set-difference (my-union
                      (children (father
                                 person))
                      (children (mother
                                 person)))
                     (list person)))

(defun my-set-difference (set1 set2)
  (cond ((null set1) nil)
        ((member (first set1)
                 set2)
         (my-set-difference (rest set1) set2))
        (t (cons (first set1)
                 (my-set-difference
                  (rest set1)
                  set2)))))

(defun my-set-remove (item set)
  (cond ((null set) nil)
        ((eq item (first set)) ; it never handles lists or numbers, so eq instead of eql
         (rest set))
        (t (cons (first set)
                 (my-set-remove item (rest set))))))

;;; :initial-value keyword is required
;;; in case (null list), otherwise the function 
(defun mapunion (fn list) ; keyword args are a boon
  (reduce #'my-union (my-mapcar fn list) :initial-value nil))

(defun mapunion (fn list)
  (reduce (lambda (x y)
            (my-union
             x
             (funcall fn y)))
          list
          :initial-value nil))

(defun my-mapunion (fn list)
  (cond ((null list) nil)
        (t (my-union (funcall fn (first list))
                     (my-mapunion fn (rest list))))))

(defun grandparents (person)
  (mapunion #'parents (parents person)))

(defun cousins (person)
  (mapunion (lambda (parent)
              (mapunion 
               #'children
               (siblings parent)))
            (parents person)))

(defun cousins-2 (person)
  (mapunion (lambda (parent)
              (reduce 
               (lambda (siblings-children
                        sibling)
                 (append (children sibling)
                         siblings-children))
               (siblings parent)
               :initial-value nil))
            (parents person)))

(defun cousins (person)
  (mapunion #'children
            (mapunion #'siblings
                      (parents person))))

;;; protects against handling incest cases incorrectly
(defun cousins (person)
  (my-set-remove 
   person
   (mapunion (lambda (parent)
               (mapunion 
                #'children
                (siblings parent)))
             (parents person))))

;;; when possible-ancestor is nil
;;; it is semantically equivalent to asking
;;; whether person has some unknown ancestor
;;; but the meaning was never explicitly defined
(defun descended-from (person possible-ancestor)
  (and person ; terminates if the person in question is uknown
       (or (null possible-ancestor) ; has an uknown ancestor
           (member 
            possible-ancestor
            (parents person))
           (descended-from
            (father person)
            possible-ancestor)
           (descended-from
            (mother person)
            possible-ancestor))))

(defun descended-from (person possible-ancestor)
  (and person ; terminates if the person in question is uknown
       (or (member ; add possible-ancestor as a clause to speed up the computation for the case possible-ancestor nil)
            possible-ancestor
            (parents person))
           (descended-from
            (father person)
            possible-ancestor)
           (descended-from
            (mother person)
            possible-ancestor))))
;;; more appropriate is direct-ancestors
(defun ancestors (person) ; unless explicitly given, this function has no chance of recursing into (ancestors nil) which is handled incorrectly
  (let ((parents (parents person)))
    (cond ((null parents)
           (list person))
          (t (my-union ; may be substituted with append because there is no circularity in the tree
              parents
              (mapunion #'ancestors parents))))))

(defun ancestors (person) ; unless explicitly given, this function has no chance of recursing into (ancestors nil) which is handled incorrectly
  (and person
       (let ((parents (parents person)))
         (cond ((null parents)
                (list person))
               (t (my-union ; may be substituted with append because there is no circularity in the tree
                   parents
                   (mapunion #'ancestors parents)))))))

;;; Returns the shortest gap, to get the longest substitute max for min
(defun generation-gap (person ancestor)
  (and person
       (let ((parents (parents person)))
         (cond ((eq person ancestor) 0)
               ((null parents) nil)
               (t (let* ((father (first parents))
                         (mother (second parents))
                         (mother-gap (generation-gap mother ancestor))
                         (father-gap (generation-gap father ancestor))
                         (shortest-gap (cond ((and mother-gap father-gap)
                                              (min mother-gap father-gap))
                                             (mother-gap mother-gap)
                                             (t father-gap))))
                    (and shortest-gap (+ 1 shortest-gap))))))))
  

;;; optimized for clarity
(defun generation-gap (person ancestor)
  (cond ((null person) nil)
        ((eq person ancestor) 0)
        ((null (parents person)) nil)
        (t (let* ((mother-gap (generation-gap
                               (mother person) ancestor))
                  (father-gap (generation-gap
                               (father person) ancestor))
                  (shortest-gap (cond ((and mother-gap father-gap)
                                       (min mother-gap father-gap))
                                      (mother-gap mother-gap)
                                      (father-gap father-gap)
                                      (t nil))))
             (and shortest-gap (+ 1 shortest-gap))))))

(defun generation-gap-2 (person ancestor)
  (generation-gap-2-rec person ancestor 0))

(defun generation-gap-2-rec (person ancestor gap)
  (cond ((null person) nil)
        ((eq person ancestor) gap)
        (t (let* ((mother-gap
                   (generation-gap-2-rec
                    (mother person)
                    ancestor
                    (1+ gap)))
                  (father-gap
                   (generation-gap-2-rec
                    (father person)
                    ancestor
                    (1+ gap)))
                  (shortest-gap
                   (cond ((and mother-gap father-gap)
                          (min mother-gap father-gap))
                         (mother-gap mother-gap)
                         (father-gap father-gap)
                         (t nil))))
             shortest-gap))))
                  

;;; (descended-from 'robert 'deirdre) => nil
;;; (ancestors 'yvette) =>
;;;(ROBERT ZELDA JULIE QUENTIN GEORGE ELLEN FRANK LINDA VINCENT BRUCE SUZANNE ARTHUR KATE COLIN DEIRDRE WANDA)
;;; (generation-gap 'olivia 'frank) => 3
;;; (cousins 'peter) => (JOSHUA ROBERT)
;;; (grandparents 'olivia) => (ANDRE HILLARY GEORGE ELLEN)
