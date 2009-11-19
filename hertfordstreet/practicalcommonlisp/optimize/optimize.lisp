(defun add (x y)
(declare (optimize (speed 3) (space 0)(safety 0)(debug 0)(compilation-speed 0)))
(declare (fixnum x y)) 
(the fixnum (+ x y)))

(disassemble 'add)

(print (add 2 5))

(print (add 2234567890 3333333333))

(print (add 1 2 3))
(print (add 1))


