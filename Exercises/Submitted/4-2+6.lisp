#|
my-copy-list uses reduce to copy a list. Reduce works by
combining two elements, the first of which acts as an
accumulator through a list, and the second of which acts
as a next pointer in a list. Consing these elements together
gives a dotted list, so the :from-end parameter is set to true.
|#
(defun my-copy-list (lst)
  (reduce #'cons lst :from-end t :initial-value '()))

#|
my-reverse uses the same strategy as my-copy-list, but uses a
lambda function to reverse the order of the input parameters.
|#
(defun my-reverse (lst)
  (reduce #'(lambda (x y) (cons y x)) lst :initial-value '()))

#|
hash-table->alist tranforms a hash table into an association
list by mapping over the hash table using maphash, and setting
a new element in the association that is a cons pair of the
hash key and hash value.
|#
(defun hash-table->alist (ht)
  (let ((al nil))
    (maphash #'(lambda (x y) (setf al (acons x y al))) ht)
    al))

#|
alist->hash-table transforms an association list into a hash
table by using mapping the car of each cons pair to the hash
key, and the cdr of each cons pair to the value.
|#
(defun alist->hash-table (al)
  (let ((ht (make-hash-table)))
    (do ((l al (rest l)))
        ((null l) ht)
      (setf (gethash (car (car l)) ht) (cdr (car l))))))