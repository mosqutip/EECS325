#|
Add dots between cons pairs in a list. show-dots simply
prints the string using format t, which removes the
quotes and prints the string like an object.
|#
(defun show-dots (lst)
  (format t "~A" (make-dotted-string lst)))

#|
make-dotted-string prints every element in a list to a
string, separating elements with dots.
|#
(defun make-dotted-string (lst)
  (cond ((null lst) nil)
        ((atom lst) lst)
        (t (format nil "(~A . ~A)"
                   (make-dotted-string (car lst))
                   (make-dotted-string (cdr lst))))))

#|
show-list prints out a list with square brackets
instead of parenthesis. Cons pairs are especially
hard to handle, and have a separate case for printing
out individual characters. Otherwise, the list is
printed character for character, with square brackets
in place of parens for every recursive list call.
|#
(defun show-list (lst)
  (cond
   ((or (null lst) (atom lst)) (princ lst))
   (t (prin1 #\[)
      (do ((l lst (rest l)))
          ((null l) (prin1 #\]))
        (cond ((atom l)
               (prin1 #\.)
               (prin1 #\space)
               (prin1 l)
               (prin1 #\])
               (return-from show-list))
              (t (show-list (car l))
                 (when (cdr l)
                   (prin1 #\space)))))))
  nil)