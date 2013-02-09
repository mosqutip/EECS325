#|(defun occurrences (lst)
  (let ((x (car lst)))
    (cond ((null lst) nil)
          (T (sort
              (let ((l (update-element x '() '())))
                (update-occurrences l (cdr lst)))
              #'> :key #'cdr)))))

(defun update-element (x lst1 lst2)
  (let ((y (car lst1)))
    (cond ((null lst1) (acons x 1 lst2))
          ((eql x (car y)) (incf (cdr y)) lst2)
          (T (update-element x (cdr lst1) lst2)))))

(defun update-occurrences (lst1 lst2)
  (let ((x (car lst2)))
    (cond ((null lst2) lst1)
          (T (update-occurrences (update-element x lst1 lst1) (cdr lst2))))))|#

(defun occurrences (lst)
  (cond ((null lst) nil)
        (T (do ((l lst (rest l))
               (l2 '()
                   (let ((x (assoc (car l) l2)))
                     (cond ((null x) (acons (car l) 1 l2))
                           (t (incf (cdr x)) l2)))))
               ((null l) (sort l2 #'> :key #'cdr))))))