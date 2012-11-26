(defun greater (x y)
  (if (>= x y)
      x
    y))

(defun has-list-p (lst)
  (cond
   ((null lst) nil)
   ((listp (car lst)) t)
   (t (has-list-p (cdr lst)))))

(defun print-dots (n)
  (do ((i 1 (1+ i)))
      ((> i n) nil)
    (format t ".")))

(defun print-dots (n)
  (cond ((= n 0) nil)
        (t (format t ".")
           (print-dots (1- n)))))

(defun get-a-count (lst)
  (do ((l lst (rest l))
       (y 0 (if (eql (car l) 'a)
                  (1+ y)
                y)))
      ((null l) y)))

(defun get-a-count (lst)
  (loop for x in lst
        count (eql x 'a)))

(defun get-a-count (lst)
  (cond ((null lst) 0)
        ((eql (car lst) 'a) (1+ (get-a-count (cdr lst))))
        (t (get-a-count (cdr lst)))))

(defun summit (lst)
  (apply #'+ (remove nil lst)))

(defun summit (lst)
  (let ((x (car lst)))
    (cond ((null lst) 0)
          ((null x) (summit (cdr lst)))
          (t (+ x (summit (cdr lst)))))))

#|(defun get-a-count (lst)
  (let ((x 0))
    (dolist (y lst)
      (if (eql y 'a)
          (incf x)
        nil))
    x))|#

#|(defun get-a-count (lst)
  (- (length lst) (length (remove 'a lst))))|#