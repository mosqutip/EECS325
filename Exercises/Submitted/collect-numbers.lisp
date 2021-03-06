(defun collect-numbers (lst)
  (cond ((listp lst)
         (mapcan (lambda (x) (cond ((numberp x) (list x))
                                   ((listp x) (collect-numbers x))
                                   (t nil)))
                 lst))
        ((numberp lst) (list lst))
        (t nil)))