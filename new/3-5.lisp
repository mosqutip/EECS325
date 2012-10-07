#|
-----Recursive definition-----
Some sort of counter is necessary to add the correct values,
so the recursive definition calls a helper function that adds
in this additional counter parameter to each call.
|#
(defun position+ (lst)
  (add-index lst 0))

(defun add-index (lst x)
  (if (null lst) nil
    (let ((y (car lst)))
      (cons (+ x y) (add-index (cdr lst) (1+ x))))))

#|
-----Iterative definition-----
A list accumulator is used to add the counter variable
to the list element. The list must be reversed at the
end because the inexpensive cons, rather than append,
is used to create the list.
|#
(defun position+ (lst)
  (do ((l lst (rest l))
       (x 0 (1+ x))
       (l2 nil (cons (+ x (car l)) l2)))
      ((null l) (reverse l2))))

#|
-----Mapcar definition-----
Since mapcar returns the last evaluated expression, the increment must
come first. Otherwise, the increment is not applied and remains -1.
|#
(defun position+ (lst)
  (let ((y -1))
    (mapcar (lambda (x) (incf y) (+ x y)) lst)))