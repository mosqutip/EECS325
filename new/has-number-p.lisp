
#|
Removing the flatten function I previously defined,
although removing the recursion in that method
necessitates two recursive calls in this function.
|#
(defun has-number-p (lst)
  (cond ((null lst) nil)
        ((atom lst) (numberp lst))
        (t (or
            (has-number-p (car lst))
            (has-number-p (cdr lst))))))