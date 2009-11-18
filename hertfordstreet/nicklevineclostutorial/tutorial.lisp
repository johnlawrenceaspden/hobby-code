(defclass point()(x y z))

(defvar my-point (make-instance 'point))

(type-of my-point)

(defun set-point-values (point a b c)
  (with-slots (x y z) point
	(setf x a y b z c)))

(set-point-values my-point 1 2 3)

(defun distance-from-origin (point)
  (with-slots (x y z)
      point
    (sqrt (+ (* x x) (* y y) (* z z)))))


(distance-from-origin my-point)

(find-class 'point)

(class-name (find-class 'point))

(class-of my-point)

(class-of (class-of my-point))

(let ((the-symbol-class (find-class 'symbol)))
  (values the-symbol-class
	  (class-name the-symbol-class)
	  (eq the-symbol-class (class-of 'symbol))
	  (class-of the-symbol-class)))

(find-class t)

(defstruct foo)

(class-of (make-foo))

(defclass daft-point ()
  ((x :accessor daft-x :initarg :x)
   (y :accessor daft-y :initform 3.14159)
   (z :reader   daft-z :allocation :class)))

(setf (slot-value (make-instance 'daft-point) 'z) 42)

(defvar my-daft-point (make-instance 'daft-point :x 19) )

(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (daft-z my-daft-point))

