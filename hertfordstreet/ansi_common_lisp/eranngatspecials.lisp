;;;;Erann Gat's essay on special variables 
;http://www.flownet.com/ron/specials.pdf


;note that an identifier can have function and value bindings (among others).
(let ((x 1))
  (flet ((x (y) (+ x y)))
    (x x)))

;scope
(defun foo ()
  (+ 
   (let ((x 1) (y 2))		       ; establish binding x*1 and y*1
     (+ x y				; refer to x*1 and y*1
	(let ((y 2) (z 3))		; establish y*2 and z*1
	  (+ x y z))))			; refer to x*1 y*2 z*1
   z))					;not z*1, but a free variable

(foo) ; an error

(defvar z 100) ; establish binding z*d1 

(foo) ; defined now (the order the code runs in is important

;;-------------------------------------------------------------------------

;lexical against dynamic scope


(defvar z 100) ; establish binding z*d1

(defun foo() (+ 1 z)) ; z is a free variable

(foo)

(defun baz ()
  (let ((z 2)) ; establish binding z*1
    (foo)))

(baz) ;what is the answer?

;;---------------------------------------------------------------------------

; It turns out there is a choice to be made. 
; Consider an imaginary lisp dialect

(defun foo () (+ z 1)) ; z is a free variable.

(defvar z 100) ; establishes dynamic binding z*d1

(l-let ((z 2))  ; establishes a lexical binding z*1
  (foo))   ; expression => 101

(d-let ((z 2))  ; establishes a dynamic binding z*d2, shadowing z*d1
  (foo))   ; free variable binds to it. expression => 3
       

; now consider 

(l-let ((z 2))           ;establishes lexical binding z*1
  (d-let ((z 3))         ;establishes dynamic binding z*d2
     z))                 ;?


;;;so we need constructs to distinguish them

(defvar z 100)  ; establish dynamic binding z*d1

(defun foo() (+ z 1)) ; no lexical binding, so this refers to z*d1

(d-let ((z 3))
  (l-let ((z 2))
	 (list (lval z) 
	       (dval z)
	       (foo))))      =>(2 3 4)


;;;;-----------------------------------------------------------------

;;;; now back to common lisp

;;;; let combines the functions of l-let and d-let using (declare special)
;;;; there is no lval, and dval is called symbol-value

(defun baz()
  (let ((x 2))
    (list x (symbol-value 'x)))) 

(baz) ; fails because there is no dynamic binding for x

(defun foo()
  (let ((x 200))
    (declare (special x)) ; establishes x*d1
    (baz)))

(foo) ; => (2 200)

(baz) ; still fails

;except just to make it even harder--------------------------
;the pervasiveness of defvar

(defvar x "doom")

(defun baz () x)

(defun foo()
  (let ((x 2))
    (baz)))

(foo) ;; => 2 based on what we know so far, this should fail

;; but it doesn't, because defvar causes all references to x to be special
;; hence the convention that all pervasively special 
;; variables should have names like *x*














 




