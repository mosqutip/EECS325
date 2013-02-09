#|
Camelize progressively scans through the string, adding letters
that aren't hyphens, and keeping track of the last character.
If the last character was a hyphen, then the next letter is capitalized.
The result is stored in a list of characters, which is later reversed
and converted to a string.
|#
(defun camelize (str &optional capitalize)
  (let ((len (1- (length str))))
    (do* ((i 0 (1+ i))
          (last (schar str 0) (schar str (1- i)))
          (current (schar str i) (schar str i))
          (final (if (eql capitalize t)
                     (list (char-upcase (schar str 0)))
                   (list (char-downcase (schar str 0))))
                 (cond ((eql #\- current) final)
                       ((eql #\- last) (cons (char-upcase current) final))
                       (t (cons current final)))))
         ((eql i len) (coerce (reverse final) 'string)))))

#|
Hyphenate progressively scans through the string, and adds
a hyphen and the next character when a case mismatch is detected.
If a value is provided for "case" that is not ":upper" or ":lower",
the program fails immediately. The result is stored in a list of
characters, which is later reversed and converted to a string.
|#
(defun hyphenate (str &optional case)
  (unless (or (eql case :upper)
              (eql case :lower))
    'error)
  (let ((len (1- (length str))))
    (do* ((i 0 (1+ i))
          (last (schar str 0) (schar str (1- i)))
          (current (schar str 0) (schar str i))
          (result (list current)
                  (if (and (lower-case-p last) (upper-case-p current))
                      (list* current #\- result)
                    (cons current result))))
         ((eql i len) (if (eql case ':lower)
                          (string-downcase (coerce (reverse result) 'string))
                        (string-upcase (coerce (reverse result) 'string)))))))