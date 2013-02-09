(defun my-remove-if (fn lst)
  (filter #'(lambda (x) (not (funcall fn x)) x) lst))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(let (max-val)
  (defun greatest-arg (&optional x)
    (unless (or (null x) (< x max-val))
      (setf max-val x))
    max-val))

(let (max-val)
  (defun greatest-arg (&optional x)
    (when (or (null x) (> max-val x))
      (setf max-val x))
    max-val))
          

(let (greater-val)
  (defun bigger-arg (&optional x)
    (when (or (null greater-val) (> x greater-val))
      (setf greater-val x))
    greater-val))

(defun memoize)

#|
(define-test greatest-arg
  (greatest-arg)
  (assert-equal -5 (greatest-arg -5))
  (assert-equal 0 (greatest-arg 0))
  (assert-equal 2 (greatest-arg 2))
  (assert-equal 2 (greatest-arg 0))
  (assert-equal 10 (greatest-arg 10))
  (greatest-arg)
  (assert-equal -5 (greatest-arg -5))
  (assert-equal 0 (greatest-arg 0))
  )

(define-test bigger-arg
  (bigger-arg)
  (assert-false (bigger-arg -5))
  (assert-true (bigger-arg 0))
  (assert-true (bigger-arg 20))
  (assert-false (bigger-arg 0))
  (assert-true (bigger-arg 5))
  (bigger-arg)
  (assert-false (bigger-arg 10))
  )
|#