#|
Return all 0s if there is no input value, otherwise create
the default list (25 10 5 1) if there is no input list.
|#
(defun make-best-change (x &optional lst)
  (cond ((or (null x) (eql x 0)) (values 0 0 0 0))
        ((null lst) (do* ((l (permute '(25 10 5 1)) (rest l))
                          (l2 (car l) (car l))
                          (y (get-coins x l2) (get-coins x l2))
                          (best (build-alist l2 y)
                                (if (and (<= (apply #'+ y) least)
                                         (<= (- x (dot-product y l2)) closest))
                                    (build-alist l2 y)
                                  best))
                          (closest (- x (dot-product y l2))
                                   (if (<= (- x (dot-product y l2)) closest)
                                       (- x (dot-product y l2))
                                     closest))
                          (least (apply #'+ y)
                                 (if (<= (apply #'+ y) least)
                                     (apply #'+ y)
                                   least)))
                         ((null l) (values-list (mapcar #'cdr (sort best #'> :key #'car))))))
        (t (do* ((l (permute lst) (rest l))
                 (l2 (car l) (car l))
                 (y (get-coins x l2) (get-coins x l2))
                 (best (build-alist l2 y)
                       (if (and (<= (apply #'+ y) least)
                                (<= (- x (dot-product y l2)) closest))
                           (build-alist l2 y)
                         best))
                 (closest (- x (dot-product y l2))
                          (if (<= (- x (dot-product y l2)) closest)
                              (- x (dot-product y l2))
                            closest))
                 (least (apply #'+ y)
                        (if (<= (apply #'+ y) least)
                            (apply #'+ y)
                          least)))
                ((null l) (values-list (mapcar #'cdr (sort best #'> :key #'car))))))))

#|
Recursively call the truncate function, to retrieve both
the quotient and remainder of coin arithmetic. The quotients
are put together in a list, which is returned and parsed
to retrieve the answers.
|#
(defun get-coins (x lst)
  (cond ((null x) nil)
        ((null lst) nil)
        (t (let ((l (multiple-value-bind (y z)
                        (truncate x (car lst))
                      (list y z))))
             (cons (car l) (get-coins (cadr l) (cdr lst)))))))

(defun get-best-coins (x lst &optional lst2)
  (if (null lst)
      (reverse lst2)
    (let* ((current (car lst))
           (l (multiple-value-bind (y z)
                  (truncate x current)
                (list y z))))
      (get-best-coins (- x (* current (1- (car l)))) (cdr lst) (cons (1- (car l)) lst2)))))

#|
Get all permutations of a list of coins (excluding a coin of value "1").
All permutations are needed to check that the solution is the best possible.
|#
(defun permute (lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) (list lst))
        (t (loop for element in lst
                 append (mapcar (lambda (l) (cons element l))
                                (permute (remove element lst)))))))

#|
Find the dot product of two vectors of indeterminate length.
|#
(defun dot-product (x y)
  (if (or (null x) (null y))
      0
    (+ (* (first x) (first y))
       (dot-product (rest x) (rest y)))))

#|
Create an association list out of two lists, associating element
l1 in list 1 with element l2 in list 2.
|#
(defun build-alist (lst1 lst2)
  (if (or (null lst1) (null lst2))
      nil
    (acons (car lst1) (car lst2)
           (build-alist (cdr lst1) (cdr lst2)))))
