#|
I'm warned by the code critic that it's bad practice to redefine
input variables, but this function necessitates the use of state
so that it can "carry" balance values between calls.
|#
(defun make-balance (x)
  #'(lambda (&optional y)
      (if (null y)
          x
        (incf x y))))