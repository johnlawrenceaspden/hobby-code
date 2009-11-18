;;;;We're trying to construct a logical inference engine
;;;;so that we can say things like:
;;(parent donald nancy)
;;(<- (child ?x ?y) (parent ?y ?x))
;;(<- (father ?x ?y) (and (parent ?x ?y) (male ?x)))
;;(<- (daughter ?x ?y) (and (child ?x ?y) (female ?x)))


;;;; pattern matching function
;; can x match y given the pre-existing bindings binds?
;; if so what bindings are needed?
;; (p ?x  b c ?x)
;; (p  a ?y c  a) match when ?x=a and ?y=b

(defun match (x y &optional binds)
  (cond 
    ;;if they're equal then just the bindings we started with
    ((eql x y) (values binds t))
    ;;if x or y are bound, replace with the binding, and try again
    ((assoc x binds) (match (binding x binds) y binds)) 
    ((assoc y binds) (match x (binding y binds) binds))
    ;;if x or y are variables, add the appropriate binding to the previous
    ((var? x) (values (cons (cons x y) binds) t))
    ((var? y) (values (cons (cons y x) binds) t))
    ;;if both x and y are lists, then match the cars and then 
    ;;try to match the cdrs with the necessary bindings for the cars added in
    ;;otherwise give up
    (t
     (when (and (consp x) (consp y))
       (multiple-value-bind (b2 yes)
	   (match (car x) (car y) binds)
	 (and yes (match (cdr x) (cdr y) b2)))))))

;;Is x like '?y' or like 'symbol'
(defun var? (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

;;What's the ultimate binding of x ?
;; (binding 'x '((x . a)(a . 2))) should be 2
;; because x is bound to a and a is bound to 2
(defun binding (x binds)
  (let ((b (assoc x binds)))
    (if b
	(or (binding (cdr b) binds)
	    (cdr b)))))

#|
;;Some test expressions

(match '(p a b c a) '(p ?x ?y c ?x))
-> ((?Y . B) (?X . A)),T

(match '(p ?x b ?y a) '(p ?y b c a))
-> ((?Y . C) (?X . ?Y)), T

(match '(a b c) '(a a a))
-> NIL

(match '(a b c) '(a b c))
-> NIL, T

; Here's an example that tries all cases:
; can we match the first two lists given that ?v must be a and ?w must be b?
; yes, and in addition we need ?x bound to c and ?y to d and ?z to e
(match '(p ?v b ?x  d (?z ?z))
       '(p  a ?w c ?y (e  e ))
       '((?v . a) (?w . b)))
-> ((?Z . E) (?Y . D) (?X . C) (?V . A) (?W . B)), T
|#

;this is where we will store our rules
(defvar *rules* (make-hash-table))

;add a new rule consequent <- antecedent to the table
(defmacro <- (con &optional ant)
  `(push (cons (cdr ',con) ',ant)
	    (gethash (car ',con) *rules*)))

(defun prove-simple (pred args binds)
  (mapcan #'(lambda (r)
	      (multiple-value-bind (b2 yes)
		  (match args (car r) binds)
		(when yes
		  (if (cdr r)
		      (prove (cdr r) b2)
		      (list b2)))))
	  (mapcar #'change-vars
		  (gethash pred *rules*))))

(defun prove (expr &optional binds)
  (case (car expr)
    (and (prove-and (reverse (cdr expr)) binds))
    (or  (prove-or (cdr expr) binds))
    (not (prove-not (cadr expr) binds))
    (t   (prove-simple (car expr) (cdr expr) binds))))

(defun change-vars (r)
  (sublis (mapcar #'(lambda(v) (cons v (gensym "?")))
		  (vars-in r))
	  r))

(defun prove-or (clauses binds)
  (mapcan #'(lambda (c) (prove c binds))
	  clauses))

(defun prove-not (clause binds)
  (unless (prove clause binds)
    (list binds)))

(defun vars-in (expr)
  (if (atom expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr))
	     (vars-in (cdr expr)))))

(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (prove ',query))
      (let ,(mapcar #'(lambda (v)
			`(,v (binding ',v ,binds)))
		    (vars-in query))
	,@body))))

(defun prove-and (clauses binds)
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b)
		  (prove (car clauses) b))
	      (prove-and (cdr clauses) binds))))

(defmacro list-relation(x)
  `(with-answer (,x ?x ?y)
    (format t "~A is the ~A of ~A~%" ?x ',x ?y)))

;-----------------------------------------------------

(defun add-large-family ()
  (<- (parent donald nancy))
  (<- (parent donald debbie))
  (<- (parent donald john))
  (<- (parent john leofric))
  (<- (parent leofric oswald))
  (<- (male donald))
  (<- (female nancy))
  (<- (female debbie))
  (<- (male john))
  (<- (male leofric))
  (<- (male oswald)))

(defun family-rules ()
  (<- (child ?x ?y) (parent ?y ?x))
  (<- (daughter ?x ?y) (and (child ?x ?y) (female ?x)))
  (<- (father ?x ?y) (and (parent ?x ?y) (male ?x)))
  (<- (grandchild ?x ?y) (and (child ?x ?z) (child ?z ?y)))
  (<- (= ?x ?x))
  (<- (sibling ?x ?y) (and (parent ?z ?x)
			   (parent ?z ?y)
			   (not (= ?x ?y))))
;;;these rules screw it all up (because of their recursion?)
  ;;(<- (descendant ?x ?y) (child ?x ?y))
  ;;(<- (descendant ?x ?y) (and (descendant ?x ?z) (descendant ?z ?y)))
  ;;(<- (parent ?x ?y) (child ?y ?x))
  )



(prog1
  (clrhash *rules*)
  (family-rules)
  (add-large-family))




#|
(prove-simple 'parent '(donald nancy) nil)

(prove-simple 'child '(?x ?y) nil)

(list-relation parent)
(list-relation child)
(list-relation daughter)
(list-relation descendant)
(list-relation sibling)
(list-relation grandchild)
|#


