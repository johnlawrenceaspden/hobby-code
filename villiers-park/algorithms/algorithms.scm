;; An Introduction to Algorithms: Shape, Space and Time

#lang racket

;; Written for PLT DrRacket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Firstly, I want to say that this talk is full of lies.
;; Adults have been lying to you all your life, and it's past time that you learned to spot when they're lying.
;; So I'm going to be very impressed and pleased if you can catch me out in any of my lies. 
;; That's a skill that's worth having if you remember nothing else from this talk.

;; Zebras are Blue

;; Isn't anyone going to pull me up on that? 
;; I mean, I've just told you that I'm going to lie to you, and
;; I've just told you that I want you to catch me out when you think I'm lying, and 
;; I've just told you a very obvious lie.

;; Surely someone is brave enough call me on it?
;; You don't have to be rude about it, or even particularly accusatory.
;; You can put your hand up politely, and you can say :
;; "Excuse me, I'm probably wrong here, but I think I remember reading somewhere that some zebras are black and white. Where am I going wrong here?".

;; OK, Zebras are a kind of space fish, which is blue, and they live on the Moon.

;; Good. At least one person here is brave enough to tell the Emperor when he's not wearing any clothes, at least once the Emperor has said it's ok.
;; But anyway, I'm going to stick in lots more lies in this talk, but they won't be as obvious. I'm going to see how many I can sneak past you.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We've written some programs in Scheme.

;; A program is a recipe for a process, and today I want to talk about the shapes of the processes that programs create as they run.

;; I'm going to give you some little programs, and you're going to play the role of the computer and execute them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First of all, let's think about factorials.

;; How many ways are there to arrange 3 things?

;; abc acb bac bca cba cab. So six. Are we done?

;; Let's get rid of c

;; ab ab ba ba ba ab

;; Every way of arranging three things is a way of arranging two things, with c put in somewhere

;; ab -> cab, acb, abc
;; ba -> cba, bca, bac

;; So the number of ways you can arrange 3 things is 3 times the number of ways you can arrange 2 things. 
;; And the number of ways you can arrange 2 things is 2 times the number of ways you can arrange 1 thing.
;; And the number of ways you can arrange 1 thing is 1.
;; ( Which is 1 times the number of ways you can arrange no things, which is also 1 )

;; This number is called the factorial, and it comes up so often that it has its own bit of notation in maths.  
;; We usually write 3! for 3 factorial.

;; 0! = 1
;; n! = n * (n-1)!

;; We can take this mathematical definition and turn it directly into a program

(define (factorial n)
  (if (= n 0) 1
      (* n (factorial (- n 1)))))

(factorial 3)

;; Now suppose you're the computer, how do you execute that program?

(factorial 3)
(if (= 3 1) 1 (* 3 (factorial (- 3 1))))
(if #f 1 (* 3 (factorial (- 3 1))))
(* 3 (factorial (- 3 1)))
(* 3 (factorial 2))
(* 3 (if (= 2 1) 1 (* 2 (factorial (- 2 1)))))
(* 3 (if #f 1 (* 2 (factorial (- 2 1)))))
(* 3 (* 2 (factorial (- 2 1))))
(* 3 (* 2 (factorial 1)))
(* 3 (* 2 1))
(* 3 2)
6


;; 12 steps

;; Now let's get rid of the book-keeping in the middle, where we do the function call and the subtraction and the if, which is 
;; the same every time, and just keep the interesting bits.

(factorial 3)
(* 3 (factorial 2))
(* 3 (* 2 (factorial 1)))
(* 3 (* 2 1))
(* 3 2)
6

;; 6 steps, because we're counting steps differently now

;; Notice how the program grows, and then when you get to (factorial 1) it bounces and starts to shrink until we're back to 1 number, which is the answer.

;; It's like it spends the first half its time planning a calculation, and the second half working it out.

;; Suppose we wanted (factorial 30), what would that look like?

;; (factorial 30)
;; ....
;; ....
;; (* 30 (* 29 (* 28 (* 27 (* .......                           (* 3 (* 2 (factorial 1))))))))
;; (* 30 (* 29 (* 28 (* 27 (* .......                           (* 3 (* 2 1)))))))
;; (* 30 (* 29 (* 28 (* 27 (* .......                           (* 3 2))))))
;; ....
;; ....
;; (* 30 8841761993739701954543616000000)
;; 265252859812191058636308480000000


;; It takes 30 steps to build up the sum, and 30 steps to collapse it down, 
;; and the longest line, in the middle, is 30 numbers long.

;; What about (factorial 300)?

;; A big number, that, but it only takes 100 times longer to compute it than it did (factorial 3), and in the middle, we need to store a big long string
;; of computations waiting to be done, but it's only 100 times longer than the string of stuff for (factorial 3)

;; We call this a linear recursion.

;; Here's the shape of a  linear recursion. It's a triangle in space and time.

;-
;---
;-----
;-------
;-----
;---
;-


;; Generally, we ignore the details about whether it's n steps, or 7*n steps, because how many steps it is depends 
;; on how you count them, and how long each step takes depends what sort of computer the program is running on.

;; So we tend to think that if the time taken is 15*n, then the 15 is sort of a fiddly detail and the n is the important bit!

;; Normally that's ok, because the difference between 15 milliseconds and 1 millisecond is not important, and 
;; neither is the difference between a million years and 15 million years.

;; We say that we need O(n) time, and O(n) space.

;; Occasionally, you'll fall into the annoying zone where it's the difference between 1 day and two weeks. 
;; At that point you do need to start caring about constant factors. But that's surprisingly rare.

;; At any rate, with an O(n) algorithm, our problem is surprisingly tractable, and we can find quite large factorials.
(factorial 1000)

;; Notice that if we stop our process in the middle, we'll need to write down quite a lot of stuff to be able to restart it from the point where it left off.

;; We can draw what's called a call graph of the process, which shows us how the data flows
'((factorial 7) --> (factorial 6) --> (factorial 5) --> (factorial 4) --> (factorial 3) --> (factorial 2) --> (factorial 1) --> (factorial 0))
'( 5040         <-- 720           <-- 120           <-- 24            <-- 6             <-- 2             <--  1            <--  1          )

(display "=========================================================================================\n")

;; Now, if we look in the middle of the factorial process, we'll find that 

(factorial 7)
;; is equal to:
(* 7 6 5 4 3 2 1)

;; So taking that as inspiration, here's a different way of computing the same function:

(define (fact n) (fact-iter n 1))

(define (fact-iter n total)
  (if (= n 0) total
      (fact-iter (- n 1) (* n total))))

(fact 3)

;; We get the same answers, but what does the computation look like now?

(fact 3)
(fact-iter 3 1)
(fact-iter 2 3)
(fact-iter 1 6)
(fact-iter 0 6)
6

;; What would (fact 30) look like now?

(fact 30)
(fact-iter 30 1)
(fact-iter 29 30)
(fact-iter 28 870)
(fact-iter 27 24360)
(fact-iter 26 657720)
;;....
;;....
(fact-iter 0 265252859812191058636308480000000)
265252859812191058636308480000000

;; We're no longer using up a lot of space storing the computations we intend to do.
;; At every step, we need only store two numbers, our counter, and our total. 
;; So it will be much easier to stop and start our computation in the middle, we only have to write down e.g.
(fact-iter 26 657720)
;; and that will carry on where we left off

;; This pattern is called a linear iteration, and it's still O(n) in time, but it's O(1) in space.

;; With an algorithm like this, it may take a long time to do the computation, but we don't really have to worry about running out of memory.

;; With the first process, we might set off a calculation that we expect to take hours, and come back later to 
;; find that the whole thing had failed because the computer had run out of space to store its computation.

;; Notice that in both cases, the programs are 'recursive' in the sense that the procedures call themselves.

;; What makes the difference is the shape that the programs make as they run.


;; Here's the shape of a linear iteration
;--
;--
;--
;--
;--
;--
;--


;; In a lot of languages, iteration has a privileged place, and it gets its own iteration construct, 
;; which might be a for/next loop, or loop/exit/end, or do/while, or while/do.

;; In some languages, that's the only sort of program you can write! 

;; Notice that we had to do some work to turn our linear recursion into a linear iteration, and it's not quite so obvious 
;; that the second program is correct and bug free.

;; There's a general pattern here. Some ideas are more natural to express recursively, and some are more natural to express iteratively.

;; If your language forces you to use iterations only, then it forces you to do some of the work that the computer should do for you.




;; We can also draw what's called a call graph of the iterative process, which shows us how the data flows
'((fact 7) --> (fact-iter 7 1) --> (fact-iter 6 7) --> (fact-iter 5 42) --> (fact-iter 4 210) --> (fact-iter 3 840) --> (fact-iter 2 1680) --> (fact-iter 1 3360) --> (fact-iter 0 3360) --> 3360)

;; Notice that the shape is kind of different and kind of the same. 
           
;; The data flows along the chain, but at the end it falls off the end, it doesn't come back, and
;; no more work is done on it once it reaches the end of the chain.

(display "=========================================================================================\n")

;; Now let's look at what is thought to be the worst example in computer science:

;; The Fibonacci Numbers are defined recursively, and they're not important enough to have their own symbol, like the factorials do, although 
;; there are plenty of cranks who will tell you that they are everywhere in Nature whooo. 

;; If you ever hear anyone talking about the mysterious wonder of the fibonnacci numbers, run.

;; FIB(0)

(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 0)
0

(fib 1)
1

(fib 2)
(+ (fib 1) (fib 0))
(+ 1 (fib 0))
(+ 1 0)
1


(fib 3)
(+ (fib 2) (fib 1))
(+ (+ (fib 1) (fib 0)) (fib 1))
(+ (+ 1 (fib 0)) (fib 1))
(+ (+ 1 0) (fib 1))
(+ 1 (fib 1))
(+ 1 1)
2

(fib 4)
(+ (fib 3) (fib 2))
(+ (+ (fib 2) (fib 1)) (fib 2))
(+ (+ (+ (fib 1) (fib 0)) (fib 1)) (fib 2))
(+ (+ (+ 1 (fib 0)) (fib 1)) (fib 2))
(+ (+ (+ 1 0) (fib 1)) (fib 2))
(+ (+ (+ 1 0) 1) (fib 2))
(+ (+ 1 1) (fib 2))
(+ 2 (fib 2))
(+ 2 (+ (fib 1) (fib 0)))
(+ 2 (+ 1 (fib 0)))
(+ 2 (+ 1 0))
(+ 2 1)
3




(define (memoize f)
  (let ((table (make-hash)))
    (lambda args
      ;; Look up the arguments.
      ;; If they're present, just give back the stored result.
      ;; If they're not present, calculate and store the result.
      ;; Note that the calculation will not be expensive as long
      ;; as f uses this memoized version for its recursive call,
      ;; which is the natural way to write it!
      (dict-ref! table args
                 (lambda ()
                   (apply f args))))))


(define mfib 
   (memoize (lambda (n)
      (if (< n 1) 1
        (+ (mfib (- n 1)) (mfib (- n 2)))))))

(/ 1 0)


(define (gcd a b)
  (print a b "-> ") 
  (cond ((= a 0) b)
        ((= b 0) a )
        ((< a b) (gcd a (remainder b a)))
        (else    (gcd (remainder a b) b))))




(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
