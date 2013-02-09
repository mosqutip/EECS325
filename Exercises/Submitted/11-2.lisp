(defclass surface ()
  ((color :accessor surface-color
          :initarg color)))

(defclass sphere (surface)
  ((radius :accessor sphere-radius
           :initarg radius
           :initform 0)
   (center :accessor sphere-center
           :initarg center
           :initform (make-instance 'point x 0 y 0 z 0))))

(defclass point ()
  ((x :accessor point-x
      :initarg x
      :initform 0)
   (y :accessor point-y
      :initarg y
      :initform 0)
   (z :accessor point-z
      :initarg z
      :initform 0)))

(defun defsphere (x y z r c)
  (let ((s (make-instance 'sphere
                          radius r
                          center (make-instance 'point x x y y z z)
                          color c)))
    (push s *world*)
    s))

(defmethod intersect ((s sphere) (pt point) xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (point-x pt) (point-x c)) xr)
                             (* (- (point-y pt) (point-y c)) yr)
                             (* (- (point-z pt) (point-z c)) zr)))
                     (+ (sq (- (point-x pt) (point-x c)))
                        (sq (- (point-y pt) (point-y c)))
                        (sq (- (point-z pt) (point-z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-instance 'point
                       x (+ (point-x pt) (* n xr))
                       y (+ (point-y pt) (* n yr))
                       z (+ (point-z pt) (* n zr))))))

(defmethod normal ((s sphere) (pt point))
  (let ((c (sphere-center s)))
    (unit-vector (- (point-x c) (point-x pt))
                 (- (point-y c) (point-y pt))
                 (- (point-z c) (point-z pt)))))