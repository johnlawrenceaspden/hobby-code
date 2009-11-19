(define (square n) (* n n))
(define (divides? a b) (= 0 (remainder b a)))

;try all divisors up to square root

(define (smallest-divisor n)
  (test-divisor n 2))

(define (test-divisor n i)
  (cond ((> (square i) n) n)
        ((divides? i n) i)
        (else (test-divisor n (+ i 1)))))

(define (smallestdivisortest n)
  (= (smallest-divisor n) n)) 


;fermat test a^n =a mod n

(define (expmod a n m)
  (cond ((= n 0) 1)
        ((even? n) (remainder (square (expmod a (/ n 2) m)) m))
        ( (remainder (* a (expmod a (- n 1) m)) m))))

(define (fermattest n a)
  (= (expmod a n n) a))

(define (randomfermattest n) (fermattest n (+ (random (- n 2)) 2)))

(define (severalrandomfermattests n k)
  (if ( = k 0) 
      #t
      (if (randomfermattest n) 
          (severalrandomfermattests n (- k 1))
          #f)))

(define (fast-prime3rand? n)(severalrandomfermattests n 3))
(define (fast-prime? n)(fermattest n 2))

;Miller-Rabin test a^(n-1) mod n = 1 but during calculation
;check whether intermediate results are 'non-trivial square roots of one'

(define (check a m)
  (cond ((= a 1) a)
        ((= a (- m 1)) a)
        ((= (remainder (square a) m) 1) 0)
        (else a)))

(define (specialexpmod a n m)
  (cond ((= n 0) 1)
        ((even? n) (check (remainder (square (specialexpmod a (/ n 2) m)) m) m))
        ( (remainder (* a (specialexpmod a (- n 1) m)) m))))  

(define (millerrabin n a)
   (specialexpmod a (- n 1) n) 1)

(define (mrtest n a)
  (= (specialexpmod a (- n 1) n) 1))

(define (millerrabintest n)
  (mrtest n 2))






(define (print-primes n m prime?)
  (if (> n m)
      (newline)
      (begin
        (if (prime? n)
            (begin (display n)(display " "))
            (display "- ")
            )
        (print-primes (+ n 1) m prime?))))

(define (printlotsofprimes n m)
  (if (> n m) 
      (newline)
      (begin
        (print-primes n (+ n 9))
        (printlotsofprimes (+ n 10) m))))







;(define (timed-prime-test n)
;  (newline)
;  (display n)
;  (start-prime-test n (process-time-clock)))
;
;(define (start-prime-test n start-time)
;  (if (smallestdivisortest n)
;      (report-prime (- (process-time-clock) start-time))))
;
;(define (report-prime elapsed-time)
;  (display " *** ")
;  (display elapsed-time))
;
;(define (searchforprimes a b)
;  (timed-prime-test a)
;  (if (< a b)  (searchforprimes (+ 1 a ) b) (newline)))
;
;(define (scan m n) (searchforprimes m (+ m n)))
;
;(define (s m n count)
;  (if (= n 0)
;    (scan m count)
;    (s (* m 10) (- n 1) count)))


(define (printftest n a)
  (display a)(display " ")(display (expmod a n n))(display " ")(display(fermattest n a)))
(define (printmrtest n a )
  (display a)(display " ")(display (specialexpmod a (- n 1) n))(display " ")(display(mrtest n a)))

(define (completefermattest n)
  (define (iter i n)
    (begin
      
      (printftest n i)
      (display "-")
      (printmrtest n i)
      (newline)
      (if (< i (- n 1)) (iter (+ i 1) n) (smallest-divisor n))))
  (iter 2 n))

(define (c n)(completefermattest n))






(define (compare a b)
  (begin (print-primes a b fast-prime?)
         (print-primes a b smallestdivisortest)
         (print-primes a b millerrabintest)))

(define (comparesome m n) (compare m (+ m (- n 1))))

(comparesome 1 50)
(comparesome 51 50)
(comparesome 101 25)
(comparesome 126 25)
(comparesome 151 25)
(comparesome 176 25)