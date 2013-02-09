#|
This doesn't work for expressions including a destructive modification,
because an expression passed to this macro must be evaluated at least
twice, and destructive modification changes the state between these
separate evaluations. I have not received clarification on this issue.
|#
(defmacro doublef (x)
  (let ((y 'y))
    `(let ((,y ,x))
       (setf ,x
             (cond ((numberp ,y) (* ,y 2))
                   ((listp ,y) (mapcar #'(lambda (z) (* z 2)) ,y))
                   ((and (vectorp ,y) (not stringp ,y)) (map 'vector #'(lambda (x) (* x 2)) ,y))
                   (t (format t "Error: unrecognized type input to doublef")))))))

(define-modify-macro doublef () (lambda (x) (* 2 x)))

(define-modify-macro doublef () f)

(setf f (lambda (x) (* 2 x)))
