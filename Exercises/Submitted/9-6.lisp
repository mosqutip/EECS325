#|
The Horner method is quite straightforward. It multiplies the
highest degree coefficient by the value of the variable, then adds
the value of the next coefficient. Since this process is symmetrical
for all elements of the input list, reduce can be used to apply the
function to each pair of elements in succession.
|#
(defun horner (x &rest coefficients)
  (reduce #'(lambda (coef1 coef2)
              (+ (* coef1 x) coef2))
          coefficients))