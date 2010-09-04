(+ 2 3)

(list 'a 1 "foo" '(b))

(cons 'a (cons 1 (cons "foo" (cons '(b) nil))))

(= x '(a b))

(= (car x) 'z)

x

(def average (x y)
	     (/ (+ x y) 2))

(average 2 4)

(fn (x y) (/ (+ x y) 2))

(= average (fn (x y) (/ (+ x y) 2)))

((fn (x y) (/ (+ x y) 2)) 3 4)


("foo" 0)

(= s "foo")

(= (s 0) #\m)

s

(let x 1
  (+ x (* x 2)))

(with (x 3 y 4) (sqrt (+ (expt x 2) (expt y 2))))

(def average (x y)
	(prn "my arguments were: " (list x y))
	(/ (+ x y) 2))

(average 100 200)

(if (odd 1) 'a 'b)
(if (odd 2) 'a 'b)

(odd 1)
(odd 2)

( if (odd 2) 'a)

(do (prn "hello")
    (+ 2 3))

(if #t (do "yo" "yo"))

(when #t
  (do 'a 'b))

(and nil
  (pr "you'll never see this"))

(def mylen (xs)
	   (if (no xs)
	       0
	(+ 1 (mylen (cdr xs)))))

(mylen nil)
(mylen '(a b))

(is 'a 'a)
(is "foo" "foo")

(let x (list 'a) (is x x))

(is (list 'a) (list 'a))

(iso (list 'a) (list 'a))

(let x 'a
  (in x 'a 'b 'c))

(def translate (sym)
	       (case sym
		 apple 'mela
		 onion 'cipolla
		 'che?))

(translate 'apple)
(translate 'onion)
(translate 'swede)

(for i 1 10 (pr i " "))
(each x '(a b c d e) (pr x " "))

(let x 10
  (while (> x 5)
	 (= x  (- x 1))
	 (pr x)))

(repeat 5 (pr "la "))

(map (fn (a) (+ a 10)) '(1 2 3) )
(map (fn (x) (+ x 10)) '(1 2 3))
(map (fn (x) (+ x 10)) '(1 2 3))

(map [+ _ 10] '(1 2 3))

(map odd:car '((1 2) (4 5) (7 9)))

(map (fn (x) (odd (car x))) '((1 2) (4 5) (7 9)))

odd:car

(map ~odd:car '((1 2) (4 5) (7 9)))

(keep odd '(1 2 3 4 5 6 7 8 9))
(rem odd '(1 2 3 4 5 6 7 8 9))

(all odd '(1 3 8 7))
(some odd '(1 3 8 7))
(pos even '(1 3 8 7))
(keep (fn (x) x) (map [ if (odd _) (+ _ 10)] '( 1 2 3 4 5 )))

(rem odd '( 1 2 3 4 5 6 ))

(all odd '( 1 2 3 4 5 6 ))

(some even '( 1 2 3 4 5 6 ))

(pos even '( 1 2 3 4 5 6 ))

(trues [if (odd _) (+ _ 10)] '(1 2 3 4 5))
(rem 'a '(a b a c u s))
(rem #\a "abacus")

(= airports (table))

(= (airports "Boston") 'bos )

(airports "Boston")

(let h (listtab '((x 1) (y 2)))
       (h 'y))

(let h (obj x 1 y 2) (h 'y))

(= codes (obj "Boston" 'bos "San Francisco" 'sfo "Paris" 'cdg))
(map codes '("Paris" "Boston" "San Francisco"))

(keys codes)
(vals codes)

(maptable (fn (k v) (prn v " " k)) codes)


codes

(= codes '(("Boston" bos) ("San Francisco" sfo) ("Paris" cdg)))
(alref codes "Boston")

(string  99 " bottles of " 'bee nil #\r)

(tostring
  (prn "domesday")
  (prn "book"))

(map type ( list 23 23.5 '(a) nil car "foo" #\a))

(coerce #\A 'int)

(let f (coerce "foo" 'cons)
       (list f
	 (car f)
	 (cdr f)
	 (type f)))

(coerce "99" 'int)
(coerce "99" 'int 16)
(coerce "99" 'int 13)

(= x '(c a b))

(pop x)
(push 'f x)

(push 'l (cdr x))

(let x '(1 2 3)
       (++ (car x))
       x)

(= x 1)
(zap [+ _ 1] x)


(= weird (fn (x y) (if (odd (- x y)) '1 (< x y))) )
(= f weird)
(= x '(1 3 6 5 3 2))
(sort f x)
(sort f x)
(zap [sort f _] x)
(zap [sort ~f _] x)
(insort f  5 x )

(sort (fn (x y) (< (len x) (len y))) '("orange" "pea" "apricot" "apple"))

(sort (compare < len) '("orange" "pea" "apricot" "apple"))

(sort (compare < len) '("orang" "pea--" "apric" "apple"))

(def greet (name (o punc))
	   (string "hello " name punc))

(greet "John")
(greet "John" "!")

(def greet (name (o punc (case name who #\? #\!)))
	   (string "hello " name punc))

(greet 'who)

(def foo (x y . z)
	 (list x y z))

(foo (+ 1 2) (+ 3 4) (+ 5 6) (+ 7 8))

(apply + '(2 3))

(def average args
  (/ (apply + args) (len args)))

(average 1 2 3)

(list '+ 1 2)

(mac foo ()
	 (list '+ 1 2))

(+ 10 (foo))

(mac when (test . body)
	  (list 'if test (cons 'do body)))

(when 1 (pr "hello ") 2)

(apply (fn (test . body)
	   (list 'if test (cons 'do body)))
       '(1 (pr "hello ") 2))

`(a b c)

(let x 2
  `(a ,x c))

(let x '(1 2)
       `(a ,x c))

(let x '(1 2)
       `(a ,@x c))

(mac when (test . body)
 `(if , test (do ,@body)))

(mac repeat (n . body)
	    `(for x 1 , n ,@body))

(repeat 3 (pr "blub "))

(let x "blub "
       (repeat 3 (pr x)))

(mac repeat (n . body)
	    `(for ,(uniq) 1 ,n ,@body))

(let x "blub "
       (repeat 3 (pr x)))


(mac do1 args
  (w/uniq g
    `(let ,g ,(car args)
	     ,@(cdr args)
	,g)))

(do1 1 2 3 4 5)

;; (mac do2 args
;;   (w/uniq g
;;     `(,(car args)
;;       (let ,g ,(cadr args)
;; 	      ,@(cddr args)
;; 	      ,g))))

;;(do2 1 2 3 4 5 6)

(mac awhen (expr . body)
	   `(let it ,expr (if it (do ,@body))))

(awhen (* 10 10) (prn (+ 25 (* it it))))

(defop hello req (pr "hello world"))



(defop hello2 req
  (w/link (pr "there")
	  (pr "here")))

(defop hello3 req
  (w/link 
    (w/link (pr "end")
       (pr "middle"))
    (pr "start")))

(defop hello4 req
  (aform [w/link (pr "you said: " (arg _ "foo"))
		 (pr "click here")]
	 (input "foo")
	(submit)))

(asv)