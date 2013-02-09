#|
Rectangle constructor can take an optional
height and an optional width argument.
|#
(defclass rectangle ()
  ((height :accessor rect-height
           :initarg height
           :initform 0)
   (width :accessor rect-width
          :initarg width
          :initform 0)))

#|
Circle constructor can take an optional radius argument.
|#
(defclass circle ()
  ((radius :accessor circ-radius
           :initarg radius
           :initform 0)))

#|
Area of a rectangle object returns height * width.
|#
(defmethod area ((x rectangle))
  (* (rect-height x) (rect-width x)))

#|
Area of a circle object returns pi * radius ^ 2.
|#
(defmethod area ((x circle))
  (* pi (expt (circ-radius x) 2)))

#|
Increment a global counter on each call to area,
using the :before macro.
|#
(defvar counter 0)

(defmethod area :before (shape)
  (incf counter))

#|
Since this assignment doesn't have any test cases, I wrote
a few of my own to verify that the code was working properly.
|#
(define-test area
  (let ((r (make-instance 'rectangle))
        (r2 (make-instance 'rectangle 'height 7 'width 6))
        (c (make-instance 'circle))
        (c2 (make-instance 'circle 'radius 13)))
    (assert-equal 5 (setf (rect-height r) 5))
    (assert-equal 3 (setf (rect-width r) 3))
    (assert-equal 15 (area r))
    (assert-equal 42 (area r2))
    (assert-equal 7 (setf (circ-radius c) 7))
    (assert-equal 153.93804002589985D0 (area c))
    (assert-equal 530.929158456675D0 (area c2))
    (assert-equal 4 counter))
)