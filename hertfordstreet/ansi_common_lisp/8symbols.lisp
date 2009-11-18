(symbol-name 'abc)

(eql 'aBc 'Abc)

(CaR '(a b c))

(list '|Lisp 1.5| '|| '|abc| '|ABC|)

(symbol-name '|a b c|)

(defun |a b c| (x)
  (* x x))

(|a b c| 2)

(get 'alizarin 'color)

(setf (get 'alizarin 'color) 'red)

(setf (get 'alizarin 'transparency) 'high)

(symbol-plist 'alizarin)

(intern "RANDOM-SYMBOL")


(defpackage "PACKAGE1"
  (:use "COMMON-LISP")
  (:nicknames "P1")
  (:export "WIN" "LOSE" "DRAW"))

(in-package PACKAGE1)

(defun win (x)
  (* x x))

(defun lose (x)
  (* x (win x)))

(defun draw (x)
  (+ (win x) (lose x)))

(defpackage "PACKAGE2"
  (:use "COMMON-LISP")
  (:nicknames "P2")
  (:export "FOO" "BAR"))

(in-package PACKAGE2)

(defun foo (x)
  (+ x x x))

(defun bar (x)
  (+ x (p1:win x))) 
