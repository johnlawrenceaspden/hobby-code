(def gcd
  (fn[a b]
    (if (= b 0)
      a
      (gcd b (mod a b)))))

(gcd 100 30) ; 10

; 100 30
; 30 10
; 10 0
; 10


;; Two registers a and b, and a temporary register t
(def a (atom 0))
(def b (atom 0))
(def t (atom 0))

(def store (fn [v n] (swap! v (fn[_] n))))

(store a 4)
(store b 3)

;; Copy value of b to a
(def fetch (fn [v] @v))

(fetch a)


(def movebtoa
  (fn[] (store a (fetch b))))

(def movettob
  (fn[] (store b (fetch t))))

;; Print values
(def dump (fn[] [@a,@b,@t]))

(dump)
(movebtoa)
(dump)

;; Get remainder (a mod b) 
(def remainder (fn[] (mod @a @b)))

(remainder)

;; put that remainder in t
(def remabtot (fn[] (store t (remainder))))

;; Test for b=0
(def isbzero? (fn[] (= @b 0)))

;; Finding the GCD of 30 and 42
(store a 30) ; 30
(store b 42) ; 42
(store t 0) ; 0
(dump) ; [30 42 0]
(isbzero?) ; false
(remabtot) ; 30
(movebtoa) ; 42
(movettob) ; 30
(dump) ; [42 30 30]
(isbzero?) ; false
(remabtot) ; 12
(movebtoa) ; 30
(movettob) ; 12
(dump) ; [30 12 12]
(isbzero?) ; false
(remabtot) ; 6
(movebtoa) ; 12
(movettob) ; 6
(dump) ; [12 6 6]
(isbzero?) ; false
(remabtot) ; 0
(movebtoa) ; 6
(movettob) ; 0
(dump) ; [6 0 0]
(isbzero?) ; true

(fetch a) ; 6

;; 30=5*6, 42=7*6

;; As well as our hardware (our data pad?)

;; We need a controller

;; loop:
;;     branch (b is zero) to done
;;     remainder to t
;;     b into a
;;     t into b
;;     goto loop
;; done

(define-machine gcd
  (registers a b t)
  (controller
   loop
   (branch (zero? (fetch b)) done)
   (assign t (remainder (fetch a) (fetch b)))
   (assign a (fetch b))
   (assign b (fetch t))
   (goto loop)
   done))
   
