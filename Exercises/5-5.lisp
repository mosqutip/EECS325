#|
Recursive routine for preceders, which builds a cons list
with successive function calls on the cdr of the list.
|#
(defun preceders (x v &key (index 0) (len (length v)) (last nil) (result nil))
  (cond ((>= index len) result)
        ((and (> index 0) (null (member last result)) (eql (aref v index) x))
         (preceders x v :index (1+ index) :len len :last (aref v index) :result (cons last result)))
        (t (preceders x v :index (1+ index) :len len :last (aref v index) :result result))))
#|
Iterative routine for preceders, which builds up a cons list based
on equality between the current element and the previous one.
|#
(defun preceders (x v)
  (if (= (length v) 0)
      nil
    (do* ((i 0 (1+ i))
          (current (aref v 0) (aref v i))
          (last nil (aref v (1- i)))
          (lst-p nil (if (and (null (member last lst-p))
                              (eql current x))
                         (cons last lst-p)
                       lst-p)))
         ((eql i (1- (length v))) lst-p))))