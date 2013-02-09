#|
rotate-array copies elements from a given array into a new array,
following the pattern: (x, y) -> (y, n-x) -> (n-x, n-y) -> (n-y, x)
-> (x, y), where n is the dimension of the square array.
|#
(defun rotate-array (arr)
  (let* ((n (car (array-dimensions arr)))
         (rotated-arr (make-array (list n n))))
    (dotimes (x n)
      (dotimes (y n)
        (setf (aref rotated-arr x y) (aref arr (1- (- n y)) x))))
    rotated-arr))

#|
nrotate-array destructively modifies the input array, following
the same pattern as rotate-array. The difference here is the use
of rotatef, which does the formula automatically. In addition,
array indices are a little tricky. The height of the rotation is
(n / 2), and the width is (n - (number of rotations * 2)), which
explains the unconventional values used for the dotimes loops.
|#
(defun nrotate-array (arr)
  (let* ((n (car (array-dimensions arr)))
         (index (1- n)))
    (dotimes (x (floor n 2))
      (dotimes (y (- index (+ (* x 2))))
        (let ((z (+ y x)))
          (rotatef (aref arr (- index z) x)
                   (aref arr (- index x) (- index z))
                   (aref arr z (- index x))
                   (aref arr x z))))))
  arr)