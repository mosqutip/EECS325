(defun tokens (str test)
  (let ((p1 (position-if (equal test str)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                  nil)))
      nil)))

(defun split-string (str &optional delim)
  (