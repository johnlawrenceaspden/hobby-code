;; Fizz Buzz

;; My sources
;; http://blog.codinghorror.com/why-cant-programmers-program/
;; inform me that:

;; The majority of computer science graduates can't solve this problem:

;; Write a program that prints the numbers from 1 to 100.  But for
;; multiples of three print "Fizz" instead of the number and for the
;; multiples of five print "Buzz". For numbers which are multiples of
;; both three and five print "FizzBuzz".

;; And Brother Downing of this parish, who actually hires people to
;; program in Java and Clojure learns me that he does indeed use this
;; to screen job applicants, and that most of them can't do it.

;; It is hard to read a thing like that without thinking: 'hang on, is that harder than it looks?'

;; So I did it, just to check:

;; I decided to use pull it out your ass driven development, where
;; you just pull the answer out of your ass.

;; First bit, print out the numbers from 1 to 100
(range 100) ;-> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 ...)

;; Bugger
(range 1 101) ;-> (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 ...)

;; paranoid now
(last (range 1 101)) ;-> 100

;; Although I guess really (print (range 1 101)) is what I want
;; here. In PIOYADD, we defer this important user interface question for later.

;; Next, print 'Fizz' instead of all the multiples of three
(map (= 0 #(% quot 3)) (range 1 101))
;; ClassCastException java.lang.Boolean cannot be cast to clojure.lang.IFn  clojure.core/map/fn--4207 (core.clj:2485)

;; bugger
(map #(= 0 (quot % 3)) (range 1 101)) ;-> (true true false false false false false false false false false false false false false false false false false false false false false false false false false ...)

;; ok
(map #(if (= 0 (quot % 3)) "Fizz" %) (range 1 101)) ;-> ("Fizz" "Fizz" 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 ...)

;; oh for fuck's sake
(map #(if (= 0 (mod % 3)) "Fizz" %) (range 1 101)) ;-> (1 2 "Fizz" 4 5 "Fizz" 7 8 "Fizz" 10 11 "Fizz" 13 14 "Fizz" 16 17 "Fizz" 19 20 "Fizz" 22 23 "Fizz" 25 26 "Fizz" ...)

;; payday
(map #(case (= 0 (mod % 3)) "Fizz" (= 0 (mod % 5)) "Buzz" %) (range 1 101))
;; IllegalArgumentException No matching clause: false  user/eval1234/fn--1235 (NO_SOURCE_FILE:1)

;; ok, I always screw that up
(map #(cond (= 0 (mod % 3)) "Fizz" (= 0 (mod % 5)) "Buzz" %) (range 1 101))
;; CompilerException java.lang.IllegalArgumentException: cond requires an even number of forms, compiling:(NO_SOURCE_PATH:1:7) 

;; twice usually
(map #(cond (= 0 (mod % 3)) "Fizz" (= 0 (mod % 5)) "Buzz" :else %) (range 1 101)) 
;-> (1 2 "Fizz" 4 "Buzz" "Fizz" 7 8 "Fizz" "Buzz" 11 "Fizz" 13 14 "Fizz" 16 17 "Fizz" 19 "Buzz" "Fizz" 22 23 "Fizz" "Buzz" 26 "Fizz" ...)

;; Bwahhahhhahhh! No, Mr Bond, I expect you to die.

;; And now, for the tricky bit of the problem, we unsheathe the
;; superweapon, copy-and-paste-driven development
(map #(cond (or (= 0 (mod % 3)) (= 0 (mod % 3))) "FizzBuzz" (= 0 (mod % 5)) "Buzz" :else %) (range 1 101)) 
;-> (1 2 "FizzBuzz" 4 "Buzz" "FizzBuzz" 7 8 "FizzBuzz" "Buzz" 11 "FizzBuzz" 13 14 "FizzBuzz" 16 17 "FizzBuzz" 19 "Buzz" "FizzBuzz" 22 23 "FizzBuzz" "Buzz" 26 "FizzBuzz" ...)

;; hmmm
(map #(cond (and (= 0 (mod % 3)) (= 0 (mod % 3))) "FizzBuzz" (= 0 (mod % 5)) "Buzz" :else %) (range 1 101)) 
;-> (1 2 "FizzBuzz" 4 "Buzz" "FizzBuzz" 7 8 "FizzBuzz" "Buzz" 11 "FizzBuzz" 13 14 "FizzBuzz" 16 17 "FizzBuzz" 19 "Buzz" "FizzBuzz" 22 23 "FizzBuzz" "Buzz" 26 "FizzBuzz" ...)

;; still wrong
(map #(cond (and (= 0 (mod % 3)) (= 0 (mod % 5))) "FizzBuzz" (= 0 (mod % 5)) "Buzz" :else %) (range 1 101)) 
;-> (1 2 3 4 "Buzz" 6 7 8 9 "Buzz" 11 12 13 14 "FizzBuzz" 16 17 18 19 "Buzz" 21 22 23 24 "Buzz" 26 27 ...)

;; aargh, where did the fizzes go?
(map #(cond (and (= 0 (mod % 3)) (= 0 (mod % 5))) "FizzBuzz" (= 0 (mod % 3)) "Fizz" (= 0 (mod % 5)) "Buzz" :else %) (range 1 101)) 
;-> (1 2 "Fizz" 4 "Buzz" "Fizz" 7 8 "Fizz" "Buzz" 11 "Fizz" 13 14 "FizzBuzz" 16 17 "Fizz" 19 "Buzz" "Fizz" 22 23 "Fizz" "Buzz" 26 "Fizz" ...)

;; Wahey! That looks done. Three minutes.

;; And now, pretending you're not a filthy hacker driven development:
(map 
 #(cond (and (= 0 (mod % 3)) (= 0 (mod % 5))) "FizzBuzz" 
        (= 0 (mod % 3)) "Fizz" 
        (= 0 (mod % 5)) "Buzz" 
        :else %) (range 1 101)) 
;-> (1 2 "Fizz" 4 "Buzz" "Fizz" 7 8 "Fizz" "Buzz" 11 "Fizz" 13 14 "FizzBuzz" 16 17 "Fizz" 19 "Buzz" "Fizz" 22 23 "Fizz" "Buzz" 26 "Fizz" ...)

(defn divides? [n m] (= 0 (mod n m)))
(divides? 3 15) ;-> false

;; sigh
(defn divides? [n m] (= 0 (mod m n)))
(divides? 3 15) ;-> true
(divides? 15 3) ;-> false
;; rah!

;; and so, behold: beauty is truth, and truth, beauty
(map 
 #(cond (divides? 15 %) "FizzBuzz" 
        (divides? 3 %)  "Fizz" 
        (divides? 5 %)  "Buzz" 
        :else %) (range 1 101)) 
;-> (1 2 "Fizz" 4 "Buzz" "Fizz" 7 8 "Fizz" "Buzz" 11 "Fizz" 13 14 "FizzBuzz" 16 17 "Fizz" 19 "Buzz" "Fizz" 22 23 "Fizz" "Buzz" 26 "Fizz" ...)

;; finally, smugness driven development:
(def fizzbuzz
  (map 
   #(cond (divides? 15 %) "FizzBuzz" 
          (divides? 3 %)  "Fizz" 
          (divides? 5 %)  "Buzz" 
          :else %) (map inc (range))))

;; There are those who would call this 'premature abstraction', but they deserve not the names of men.
(print (take 100 fizzbuzz))
;; (1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz Fizz 22 23 Fizz Buzz 26 Fizz ...)

;; And I suppose a regression test would be nice, if I'm trying to
;; give some sort of professional impression:
(= 
 (take 27 fizzbuzz)
 (list 1 2 "Fizz" 4 "Buzz" "Fizz" 7 8 "Fizz" "Buzz" 
       11 "Fizz" 13 14 "FizzBuzz" 16 17 "Fizz" 19 "Buzz"
       "Fizz" 22 23 "Fizz" "Buzz" 26 "Fizz"))

;; And some paranoid checks, if I'm going to put this travesty on my blog:
(count (filter #(= "Fizz" %) (take 1000 fizzbuzz))) ; -> 267
(count (filter #(= "FizzBuzz" %) (take 1000 fizzbuzz))) ;-> 66
(count (filter #(= "Buzz" %) (take 1000 fizzbuzz))) ;-> 134

(* (+ 134 66) 5) ;-> 1000
(* (+ 267 66) 3) ;-> 999

;; bah, that's close enough for government work. I declare myself
;; done. Three minutes of flail and two minutes of tidying up and
;; checking it works.

;; So my question to the wider community is: Does my three minutes of
;; flailing trying to remember the semantics of my favourite language
;; (which I can perfectly imagine looking dreadful at an interview)
;; count as a fail or a pass?

;; In a previously unknown assembly language, but given a library to
;; print numbers on the screen, I can imagine taking half an hour to
;; get this working. Without the library, it's a research project.

;; And obviously, if I tried to do it in Haskell, I'd have to spend
;; three weeks remembering what a monad was in order to print the
;; damned thing out, but at least the type system would magically
;; guarantee the correctness of the final program.

;; In other words, is fizzbuzz really actually quite hard, or is
;; everyone out there a complete idiot?






