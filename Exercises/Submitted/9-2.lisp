#|
Return all 0s if there is no input value, otherwise create
the default list (25 10 5 1) if there is no input list.
|#
(defun make-change (x &optional lst)
  (cond ((null x) (values 0 0 0 0))
        ((null lst) (values-list (get-coins x '(25 10 5 1))))
        (t (values-list (get-coins x lst)))))

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