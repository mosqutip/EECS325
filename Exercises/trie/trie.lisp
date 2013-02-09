(defclass trie ()
  ((val :initform nil
        :accessor trie-val)
   (branches :initform (make-hash-table)
             :accessor trie-branches)))

(defun add-word (str trie)
  (add-word-with-offset str trie 0))

(defun add-word-with-offset (str trie off)
  (with-slots (value branches) trie
    (if (= off (length str))
        (progn
          (setf (slot-value trie 'val) (string-downcase str))
          trie)
      (let ((key (intern (string-downcase (char str off)))))
        (unless (gethash key branches)
          (setf (gethash key branches) (make-instance 'trie)))
        (add-word-with-offset str (gethash key branches) (1+ off))))))
  
(defun subtrie (trie &rest charlist)
  (with-slots (value branches) trie
    (if (null (car charlist))
        trie
      (let ((key (intern (string (car charlist)))))
        (if (gethash key branches)
            (multiple-value-call #'subtrie
              (nth-value 0 (gethash key branches))
              (values-list (rest charlist)))
          (return-from subtrie nil))))))

(defun trie-word (trie)
  (unless (null (slot-value trie 'val))
    (slot-value trie 'val)))

(defun trie-count (trie)
  (let ((count 0))
    (trie-count-helper trie count)))

(defun trie-count-helper (trie count)
  (if (null trie)
      0
    (with-slots ((b branches) (v val)) trie
      (print v)
      (if (null b)
          (if (null v)
              0
            1)
        (+ count (maphash #'(lambda (key val)
                              (trie-count-helper val count))
                     b))))))

(defun mapc-trie (fn trie))

(defun read-words (file trie)
  (with-open-file (stream file)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (add-word line trie))))

(let ((x 0))
  (with-hash-table-iterator (iter h)
    (loop (multiple-value-bind (more key value) (iter)
            (unless more (return x))
            (incf x) (print value)))))

(let ((x 0))
  (with-hash-table-iterator (iter h)