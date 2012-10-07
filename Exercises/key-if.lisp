#|(defmacro key-if (test &optional then-form else-form)
  `(cond
    (,test ,x)
    (t ,y)))|#

(defmacro key-if (test &optional lst1 lst2)
  (let ((then-expr (if (eql (car lst1) ':then) (rest lst1)
                     (rest lst2)))
        (else-expr (if (eql (car lst1) ':else) (rest lst1)
                     (rest lst2))))
    (list 'cond
          (list test then-expr)
          (list t else-expr))))

(defmacro key-if (test &optional &rest args)
  (cond ((null args) nil)
        (t (