(defun show-dots (lst)
  (if (null lst)
      nil
    (format t "~A" (cons (car lst) (show-dots (cdr lst))))))

(defun show-list)