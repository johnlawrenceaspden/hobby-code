(load "unittests.lisp")
(load "jclos.lisp")

;;duck typing with structures
(defstruct srectangle
  height width)

(defstruct scircle
  radius)

(defun sarea (x)
  (cond ((srectangle-p x)
	 (* (srectangle-height x) (srectangle-width x)))
	((scircle-p x)
	 (* pi (expt (scircle-radius x) 2)))))

(deftest (mapcar #'sarea (list (make-srectangle :height 2 :width 3)
			      (make-scircle :radius 2))) 
    (6 12.566370614359172d0))


;;defclass makes the whole thing more complicated

(defclass colored()
  (color))

(defclass rectangle ()
  (height width))

(defclass circle ()
  ((radius :accessor circle-radius
	   :initarg :radius
	   :initform 1)
   (centre :accessor circle-centre
	   :initarg centre
	   :initform (cons 0 0))))

(defclass colored-circle (circle colored)
  ())

(defmethod area (( x rectangle))
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod area ((x circle))
  (* pi (expt (slot-value x 'radius) 2)))

(deftest 
    (let ((r (make-instance 'rectangle)))
	   (setf (slot-value r 'height) 2
		 (slot-value r 'width)  3)
	   (area r))
    6)

(deftest
    (let ((c (make-instance 'colored-circle :radius 10)))
      (area c))
    314.1592653589793d0)

;;obviously as a second example I feel the need to redefine the class system
;;using macros so that it's a bit more like the nice structure examples.
;;those macros are now moved out to jclos.lisp
(define-class polygon ()
  ((colour 'red)))

(define-class triangle (polygon)
  (a b c))

(deftest 
    (let ((tri (make-instance 'triangle :a 3 :b 4 :c 5)))
      (polygon-colour tri))
    RED)

(runtests)

(setf a (make-triangle))
(triangle-a a)

;;;;shared slots
(defclass tabloid ()
  ((top-story :accessor story
	      :allocation :class)))

(setf daily-blab       (make-instance 'tabloid)
      unsolicited-mail (make-instance 'tabloid))

(setf (story daily-blab) 'adultery-of-senator)
(story unsolicited-mail)

(defclass broadsheet ()
  ((top-story :accessor story
	      :allocation :class)))

(setf snooty-times (make-instance 'broadsheet)
      lefty-pap    (make-instance 'broadsheet))

(setf (story snooty-times) 'financial-tedium)
(story lefty-pap)

;;;;;;11.4 superclasses


(defclass graphic ()
  ((color :accessor graphic-color :initarg :color)
   (visible :accessor graphic-visible :initarg :visible :initform t)))

(define-class graphic () (color (visible t)))

(define-class screen-circle (circle graphic)())

(graphic-color (make-instance 'screen-circle :color 'red :radius 3))

(defclass screen-circle (circle graphic)
  ((color :initform 'purple)))


(graphic-color (make-instance 'screen-circle))

(setf a (make-instance 'screen-circle))

(graphic-color a)

;;;;11.5 precedence

(defclass art-thing (thingy)())

(defclass sculpture () (height width depth (number :initform 'sc)))

(defclass statue (sculpture) (subject))

(defclass metalwork () (metal-type (number :initform 'mw)))

(defclass casting (metalwork) ())

(defclass cast-statue (statue casting) ())

;precedence order cast-statue->statue->sculpture->casting->metalwork->art-thing->()

(slot-value (make-instance 'cast-statue) 'number) ; sc

;;;;11.6 Generic Functions

(defmethod combine (x y)
  (list x y))

(combine 'a 'b)

(defclass stuff () ((name :accessor name :initarg :name)))

(defclass ice-cream (stuff) ())
(defclass topping   (stuff) ())

(defmethod combine ((ic ice-cream) (top topping))
  (format nil "~A ice-cream with ~A topping."
	  (name ic)
	  (name top)))

(defmethod combine ((ic1 ice-cream) (ic2 ice-cream))
  (list 'double-cone (name ic1) (name ic2)))

(defmethod combine ((ic ice-cream) x)
  (format nil "~A ice-cream with ~A." (name ic) x))

(defmethod combine ((x number) (y number))
  (+ x y))

(defmethod combine ((x (eql 'powder)) (y (eql 'spark))) 'boom)
(defmethod combine ((y (eql 'spark)) (x (eql 'powder))) 'kaboom)

(combine (make-instance 'ice-cream :name 'fish) 'reluctance)

(combine (make-instance 'ice-cream :name 'grape) 
	 (make-instance 'topping   :name 'marshmallow))

(combine (make-instance 'ice-cream :name 'fig)
	 (make-instance 'topping   :name 'treacle))

(combine (make-instance 'ice-cream :name 'doom)
	 (make-instance 'ice-cream :name 'horror))

(combine 3 4)

(combine 'powder 'spark)
(combine 'spark 'powder)

;;;;11.7 Auxiliary Methods

(defclass speaker () ())

(defmethod speak ((s speaker) string)
  (format t "~A" string))

(speak (make-instance 'speaker) "I'm hungry") 

(defclass intellectual (speaker) ())
(defmethod speak :before ((i intellectual) string)
  (princ "Perhaps "))
(defmethod speak :after ((i intellectual) string)
  (princ " in some sense"))

(speak (make-instance 'intellectual) "I'm hungry")

(defmethod speak :before ((s speaker) string)
  (princ "I think "))

(defclass courtier (speaker) ())

(defmethod speak :around ((i courtier) string)
  (format t "Does the king believe that ~A? " string)
  (if (eql (read) 'yes)
      (if (next-method-p) (call-next-method))
      (format t "Indeed, it is a preposterous idea.~%")))

(speak (make-instance 'courtier) "kings will last")
(speak (make-instance 'courtier) "the world is round")


;;;;11.8 method-combination
(defgeneric price (x)
  (:method-combination +))

(defclass jacket () ())
(defclass trousers ()())
(defclass suit (jacket trousers) ())

(defmethod price + ((jk jacket)) 350)
(defmethod price + ((tr trousers)) 200)

(price (make-instance 'suit))

;;;;11.9 Encapsulation

(defpackage "CTR"
  (:use "COMMON-LISP")
  (:export "COUNTER" "INCREMENT" "CLEAR"))

(in-package ctr)

(defclass counter () ((state :initform 0)))
(defmethod increment ((c counter))(incf (slot-value c 'state)))
(defmethod clear ((c counter)) (setf (slot-value c 'state) 0))
(unintern 'state)  ;makes it so that even ctr::state doesn't work

(in-package "COMMON-LISP-USER")

(setf a (make-instance 'ctr:counter))
(ctr:increment a)
(ctr:clear a)

(slot-value a 'ctr::state)



(intern "STATE")







