#|(defmacro key-if (test &optional then-form else-form)
  `(cond
    (,test ,x)
    (t ,y)))|#

(defmacro key-if (test &key ((:then then-expr)) ((:else else-expr)))
  (list 'cond
        (list test then-expr)
        (list t else-expr)))

(defmacro key-if (test &key ((:then then-expr)) ((:else else-expr))) (list then-expr else-expr)
  (list 'cond
        (list test then-expr)
        (list t else-expr)))
#|
(cond
 ((test 1) result)
 ((test 2) result)
 .
 .
 .
 (t result))
|#




#|(defmacro key-if (test &rest exps &aux (then ()) (else ()))
  (if (not (keywordp (first exps))) (error "the first expression must be a keyword"))
  (let ((thenp t))
    (dolist (e exps)
      (if (keywordp e)
          (cond ((eql e :then) (setf thenp t))
                ((eql e :else) (setf thenp nil))
                (t (error "the keyword ~a is unknown" e))
                )
        (if thenp (setf then (append then `(,e))) (setf else (append else `(,e))))
        )
      )
    )
  (if (eql then nil) (setf then '(nil)))
  (if (eql else nil) (setf else '(nil)))
  `(cond (,test ,@then)
         (t ,@else)
         )
  )|#