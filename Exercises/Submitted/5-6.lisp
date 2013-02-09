#|
Recursive definition of intersperse.
|#
(defun intersperse (x lst)
  (if (null (cdr lst))
      lst
    (list* (car lst) x (intersperse x (cdr lst)))))

#|
Iterative definition of intersperse.
|#
(defun intersperse (x lst)
  (if (null lst)
      nil
    (do ((l (rest lst) (rest l))
         (l2 (list (car lst)) (list* (car l) x l2)))
        ((null l) (reverse l2)))))