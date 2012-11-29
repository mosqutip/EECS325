#|
Recursive definition of diff-by-one-p
|#
(defun diff-by-one-p (lst)
  (cond ((null (cdr lst)) t)
        ((null (cadr lst)) 'error)
        ((or (eql (1+ (car lst)) (cadr lst))
             (eql (1- (car lst)) (cadr lst)))
         (diff-by-one-p (cdr lst)))
        (t nil)))

#|
Iterative definition of diff-by-one-p
|#
(defun diff-by-one-p (lst)
  (do* ((l lst (rest l))
        (current (car l) (car l)))
       ((null (cdr l)) t)
    (when (null (cadr l))
      (return 'error))
    (unless (or (eql (1+ current) (cadr l))
                (eql (1- current) (cadr l)))
      (return nil))))

#|
Mapc definition of diff-by-one-p
|#
(defun diff-by-one-p (lst)
  (mapc #'(lambda (x y)
            (if (null y)
                (return-from diff-by-one-p 'error)
              (unless (or (eql (1+ x) y)
                          (eql (1- x) y))
                (return-from diff-by-one-p nil))))
        lst (cdr lst))
  t)

#|
Every definition of diff-by-one-p
|#
(defun diff-by-one-p (lst)
  (every #'(lambda (x y)
             (if (null y)
                 (return 'error)
               (or (eql (1+ x) y)
                   (eql (1- x) y))))
         lst (cdr lst)))