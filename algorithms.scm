;; So today I want to talk about algorithms.

;; An algorithm is pretty much 'anything you can do with a computer', and so the study of algorithms is very important to computer science, and to the wider world.

;; I want to talk about the shape of computations in time and space

;; One of the simplest things we can compute is 'the number of ways of arranging some things'

;; How many ways can we arrange 3 things?

;; {a}
;;  a

;; now add b, we can put it before a, or after a
;; ba
;; ab

;; now add c, we can put it before, in the middle, or at the end, and we can do that for each way of arranging two things
;; cba
;; bca
;; bac
;; cab
;; acb
;; abc

;; Does everyone know the factorial function? 

;; n! = n * (n-1)!

;; 5! = 5 * 4! = 5 * 4 * 3! ...

;; Imagine we had a program for this:
;; (fact 0) ;-> 1
;; (fact n) ;-> (* n (fact (- n 1))

;; Can we write a program for this?

(define fact
  (lambda (n)
    (if (= n 0) 1
        (* n (fact (- n 1))))))

(fact 3)

;; Now the question is, what does the algorithm represented by our program look like as it executes?

(fact 3)
((lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))) 3)        ; get function
(if (= 3 0) 1 (* 3 (fact (- 3 1))))                         ; argument substitution
(if #f 1 (* 3 (fact (- 3 1))))                              ; evaluate conditional
(* 3 (fact (- 3 1)))                                        ; choose branch
(* 3 (fact 2))                                              ; subtract
(* 3 ((lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))) 2))   
(* 3 (if (= 2 0) 1 (* 2 (fact (- 2 1)))))
(* 3 (if #f 1 (* 2 (fact (- 2 1)))))
(* 3 (* 2 (fact (- 2 1))))
(* 3 (* 2 (fact 1)))
(* 3 (* 2 ((lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))) 1)))
(* 3 (* 2 (if (= 1 0) 1 (* 1 (fact (- 1 1))))))
(* 3 (* 2 (if #f 1 (* 1 (fact (- 1 1))))))
(* 3 (* 2 (* 1 (fact (- 1 1)))))
(* 3 (* 2 (* 1 (fact 0))))
(* 3 (* 2 (* 1 ((lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))) 0))))
(* 3 (* 2 (* 1 (if (= 0 0) 1 (* 0 (fact (- 0 1)))))))
(* 3 (* 2 (* 1 (if #t 1 (* 0 (fact (- 0 1)))))))
(* 3 (* 2 (* 1 1 )))
(* 3 (* 2 1))
(* 3 2)
6

;; Now let's cut out some of the function call machinery, so that we can see a bit more clearly what's going on here:

(fact 3)
(* 3 (fact 2))
(* 3 (* 2 (fact 1)))
(* 3 (* 2 (* 1 (fact 0))))
(* 3 (* 2 (* 1 1 )))
(* 3 (* 2 1))
(* 3 2)
6

;; We have a process that grows, and then collapses.

;; How long is it from top to bottom?

;; How long is the longest line?

;; This is called a 'linear recursion'

;; Suppose instead we wanted to know the factorial of 30. How many ways are there of arranging 30 things?

(fact 30)

;; We'd expect the execution to be 10 times as long, and the long list in the middle to be 10  times as long too.

;; (* 30 (* 29 (* 28 (* 27 (* 26 (* ......))))))

;; Can anyone think of a different way to compute this number?

;; What would we do if we were trying to compute the factorial of 6 ourselves?

;; I'm not sure that I'd write down: 

(* 6 (* 5 (* 4 (* 3 (* 2 (* 1))))))
(* 6 (* 5 (* 4 (* 3 (* 2 1)))))
(* 6 (* 5 (* 4 (* 3 2))))
(* 6 (* 5 (* 4 6)))
(* 6 (* 5 24))
(* 6 120)
(* 720)



;; I'd go

;; 1: 1
;; 2: 2
;; 3: 6
;; 4: 24
;; 5: 120
;; 6: 720

;; Can we make a computation that works like that?

(define fact-iter 
  (lambda (n count total)
    (if (= count n) total
        (fact-iter n (+ 1 count) (* count total)))))


(fact-iter 6 1 1)
((lambda (n count total) (if (= count n) total (fact-iter n (+ 1 count) (* count total)))) 6 1 1)
(if (= 1 6) 1 (fact-iter 6 (+ 1 1) (* 1 1)))
(if #f 1 (fact-iter 6 (+ 1 1) (* 1 1)))
(fact-iter 6 (+ 1 1) (* 1 1))
(fact-iter 6 2 1)
((lambda (n count total) (if (= count n) total (fact-iter n (+ 1 count) (* count total)))) 6 2 1)
(if (= 2 6) 1 (fact-iter 6 (+ 1 2) (* 2 1)))
(if #f 1 (fact-iter 6 (+ 1 2) (* 2 1)))
(fact-iter 6 (+ 1 2) (* 2 1))
(fact-iter 6 3 2)
;; and so on

(fact-iter 6 1 1)
(fact-iter 6 2 1)
(fact-iter 6 3 2)
(fact-iter 6 4 6)
(fact-iter 6 5 24)
(fact-iter 6 6 120)
720

;; In this version, the computation doesn't grow. There are always three numbers every time we get to the function call.

;; This pattern is called an iteration


;; Or maybe 
;; 6 1
;; 5 6
;; 4 30
;; 3 120
;; 2 360
;; 1 720
;; 720

(define (fact-iter n total)
  (if (= n 1) total
      (fact-iter (- n 1) (* n total))))

(define (factorial n) (fact-iter n 1))



FUNCTION factorial(n)
  total = 1
  count = 1
  WHILE (count <= n)
    total = total*count
    count = count + 1
  END
  RETURN total
END

int factorial (int n)
{
 int count;
 int total;
 for (count=1, total=1; count <=n ; count ++)
 {
     total = count*total; 
 }
 return total;
}

(define (fact-iter n count total)
    (if (= count n) total
        (fact-iter n (+ 1 count) (* count total))))

(define (factorial n) (fact-iter n 1 1))


FUNCTION factorial (n)
  total = 1 
  FOR count = 1 to n
    total = count * total 
  NEXT count
  RETURN total
END

(reduce * (range 1 (inc n)))

int factorial-iter (int n, int count, int total)
{
  if (count < n)
  {
     return factorial-iter (n, count + 1, total * count);
  }
  else
  {
     return total;
  }
}

int factorial (int n)
{
   return factorial-iter (n, 1, 1);
}






