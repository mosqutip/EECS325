(defun car-circular-p (ls)
  (eq ls (car ls)))

(defun cdr-circular-p (ls)
  (labels ((rec (ls1)
             (if ls1
                 (or (eq (cdr ls1) ls)
                     (rec (cdr ls1))))))
    (rec ls)))

#|
(define-test car-circular-p
  (let ((*print-circle* t))
    (assert-false (car-circular-p '(1 2 3 4)))
    (assert-false (car-circular-p '#1=(1 2 3 4 . #1#)))
    (assert-false (car-circular-p '((1 2) 3 4)))
    (assert-true (car-circular-p '#2=(#2#)))
    (assert-true (car-circular-p '#3=((1 2) 3 #3#)))
    (assert-false (car-circular-p '(#4=(1 2) 3 #4#)))
    (assert-false (car-circular-p '(((1 2 3)) 4 ((5 6)))))
    (assert-false (car-circular-p '(((1 2 . #5=(3))) 4 ((5 . #5#)))))
    (assert-false (car-circular-p '(#6=((1 2 3)) 4 ((5 . #6#)))))
    (assert-true (car-circular-p '(((1 2 3)) 4 . #7=(((5 . #7#))))))
    (assert-true (car-circular-p '#8=(1 2 (((3 #8#))) 4)))
    (assert-false (car-circular-p '(#9=(1 2 3 . #9#))))
    (assert-true (car-circular-p '#10=(#11=((1 2) 3 #11#) 2 3 . #10#)))
    (assert-true (car-circular-p '#12=(a . ((b . #12#) . c))))
    (assert-true (car-circular-p '((#13=(1 2 . #13#)) #14=((3 . #14#)))))
    ))
|#