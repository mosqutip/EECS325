#|
Tokenizer class holds one element: a list of strings
that have been tokenized by the given delimiter.
|#
(defclass tokenizer ()
  ((tokens :initform nil
           :accessor tkns)))

#|
next-token-p returns true if there is still a token left
in the list, and false if there isn't.
|#
(defmethod next-token-p ((tokenizer tokenizer))
  (not (null (slot-value tokenizer 'tokens))))

#|
next-token returns the next token in the list, and sets the
list to the cdr of the list, destructively modifying it.
|#
(defmethod next-token ((tokenizer tokenizer))
  (let ((slot (slot-value tokenizer 'tokens)))
    (setf (slot-value tokenizer 'tokens) (cdr slot))
    (car slot)))

#|
split-on-delim tokenizes a string based on an optional delimiter,
which defaults to space. The special case here is for spaces, the
default, since multiple spaces are not considered actual terms like
multiples of other delimiters. Spaces are collapsed into one space,
then treated like regular delimiters. 
|#
(defun split-on-delim (str delim)
  (do* ((start 0 (1+ end))
        (end (position delim str) (position delim str :start start))
        (result (if (and (eql delim #\space) (eql start end))
                    nil
                  (list (subseq str start end)))
                (if (and (eql delim #\space) (or (eql start end) (null end)))
                    result
                  (cons (subseq str start end) result))))
       ((null end) (reverse result))))

#|
make-tokenizer instantiates a new tokenizer object, and assigns the
tokenized string based on the delimiter to the object's tokens field.
|#
(defun make-tokenizer (str delim)
  (let ((tr (make-instance 'tokenizer)))
    (setf (slot-value tr 'tokens) (split-on-delim str delim))
    tr))

#|
split-string is the wrapper given by the example code.
|#
(defun split-string (str &optional (delim #\space))
  (let ((tknzr (make-tokenizer str delim)))
    (do ((l nil (cons (next-token tknzr) l)))
        ((not (next-token-p tknzr)) (nreverse l)))))