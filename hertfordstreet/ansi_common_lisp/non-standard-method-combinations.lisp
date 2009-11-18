(defgeneric point= (a b)
  (:method-combination and))

(defclass point()
  ((x :initarg :x :reader point-x)
   (y :initarg :y :reader point-y)))

(defun make-point (x y)
  (make-instance 'point :x x :y y))

(defmethod point= and ((a point) (b point))
  (and (= (point-x a) (point-x b))
       (= (point-y a) (point-y b))))

(defclass color-point (point)
  ((color :initarg :color :reader point-color)))

(defun make-color-point (x y c)
  (make-instance 'color-point :x x :y y :color c))

(defmethod point= and ((a color-point) (b color-point))
  (string= (point-color a) (point-color b)))

(point= (make-color-point 1 1 "red")
	(make-color-point 1 1 "red"))

(point= (make-point 10 10) (make-point 10 10))

(point= (make-color-point 10 10 "red") (make-point 10 10))


(defmethod point= and ((a color-point) (b point)) nil)
(defmethod point= and ((a point) (b color-point)) nil)


	   