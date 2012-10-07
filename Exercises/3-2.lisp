(defun stable-union (l1 l2)
  (cond
   ((and (null l1) (null l2)) nil)
   ((null l1) l2)
   ((null l2) l1)
   ((eql l1 l2) l1)
   (t (append l1 (stable-set-difference l2 l1)))))

(defun stable-intersection (l1 l2)
  (cond
   ((null l1) nil)
   ((null l2) nil)
   ((eql l1 l2) l1)
   (t (loop for x in l1
            when (member x l2)
            collect x))))

(defun stable-set-difference (l1 l2)
  (cond
   ((null l1) nil)
   ((null l2) l1)
   ((eql l1 l2) nil)
   (t (loop for x in l1
            unless (member x l2)
            collect x))))