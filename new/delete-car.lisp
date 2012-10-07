(defun delete-car (l)
  (cond ((null (cdr l)) (setf l nil))
        (T (setf (first l) (second l) (cdr l) (cddr l))))
  l)