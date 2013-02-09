#|
preserve works using by exploiting dynamic scoping. Variables
defined within a lambda expression are local to that lambda
only, such that, after leaving the lambda function, the variables
return to their definition in the outer scope (if defined).
|#
(defmacro preserve (var-lst &body body)
  `((lambda ,var-lst ,@body) ,@var-lst))