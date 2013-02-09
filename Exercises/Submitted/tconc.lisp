#|
A tconc structure has head (list) and tail (cdr) pointers.
The constructor keyword is necessary because of the
redefinition of make-tconc required by the specifications.
|#
(defstruct (tconc (:constructor make-new-tconc))
  (head nil)
  (tail nil))

#|
make-tconc creates a new tconc structure, and initializes
it with the appropriate values of lst, if provided.
|#
(defun make-tconc (&optional lst)
  (let ((tcnc (make-new-tconc)))
    (setf (slot-value tcnc 'head) lst)
    (setf (slot-value tcnc 'tail) (last lst))
    tcnc))

#|
tconc adds the given items to the tconc structure and returns
the list held within. If no items are given, the list held
in the tconc structure is returned without modification.
|#
(defun tconc (tconc-structure &rest items)
  (or (null items)
      (cond ((null (tconc-head tconc-structure))
             (setf (tconc-head tconc-structure) items)
             (setf (tconc-tail tconc-structure) (last items)))
            (t (setf (cdr (tconc-tail tconc-structure)) items)
               (setf (tconc-tail tconc-structure) (last items)))))
  (tconc-head tconc-structure))

#|
tconc-list adds the given list to the tconc structure and
returns the list held within. If no list is given, the list
held in the tconc structure is returned without modification.
Because of the &rest parameter used in tconc, this function
is simply an application of tconc.
|#
(defun tconc-list (tconc-structure &optional lst)
  (apply #'tconc tconc-structure lst))