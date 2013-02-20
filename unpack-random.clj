;; Packing and Unpacking Numbers

;; This post is kind of pointless, since while writing it I discovered a better way to do the same thing.

;; But I also think it's kind of cool, so I'm putting it up in case anyone else finds a use for it.

;; Suppose we've got a function which gives random results (I mean, deliberately...)

(defn randomfunction []
  (list 
   (case (rand-int 2) 0 '+ 1 '-)
   (rand-int 100)
   (list 
    (case (rand-int 2) 0 '+ 1 '-)
    (rand-int 100)
    (rand-int 100))))

(randomfunction) ;-> (+ 43 (- 71 75))
(randomfunction) ;-> (- 18 (- 13 22))
(randomfunction) ;-> (- 36 (+ 60 3))
(randomfunction) ;-> (+ 56 (+ 34 4))
(randomfunction) ;-> (- 87 (- 61 76))

;; And it comes to testing it:

(= (randomfunction) '(+ 26 (- 69 0))) ;-> false

;; We'd prefer it to be deterministic. 

;; So we can separate the concerns thus:

(defn deterministicfunction [a b c d e]
  (list 
   (case a 0 '+ 1 '-)
   b
   (list 
    (case c 0 '+ 1 '-)
    d
    e)))

(defn randomfunction []
  (deterministicfunction (rand-int 2) (rand-int 100) (rand-int 2) (rand-int 100) (rand-int 100)))

;; tadaa!

(= (deterministicfunction 0 26 1 69 0) '(+ 26 (- 69 0))) ;-> true

(randomfunction) ;-> (+ 30 (+ 52 95))
(randomfunction) ;-> (- 97 (- 57 61))
(randomfunction) ;-> (+ 88 (+ 26 15))

;; Although useful, this is a right royal pain in the neck, and horribly error-prone

;; It occurred to me that it might be made less painful by setting the
;; seed of the random number generator before running tests, but I
;; also had a need to make 'deterministic random' values in the code
;; itself, and having everything hanging off a global variable that
;; any old function occasionally resets struck me as a recipe for
;; disaster.

;; So I thought, well why not pack all the random parameters into one big number:

(def << bit-shift-left)

;; If all the random ranges are powers of two then this is easy:

(let [[a b c d] [(rand-int 8) (rand-int 4) (rand-int 8) (rand-int 2)]]
 [[a b c d] (+ (<< a (+ 1 3 2)) (<< b (+ 1 3)) (<< c 1) (<< d 0))]) 
; [[2 2 4 0] 168] 
; [[3 1 4 0] 216] 
; [[5 3 6 0] 380]

;; Looking at the last one
;; 5/3/6/0 is 
;; 101/11/110/0 in binary

;; and binary 101111100 is:
2r101111100 ; 380

;; in decimal

;; To extract the numbers, we can do:

(mod 380 2) ; 0 
(/ (- 380 (mod 380 2)) 2) ; 190 
(mod 190 8) ; 6 
(/ (- 190 (mod 190 8)) 8) ; 23
(mod 23 4) ; 3
(/ (- 23 (mod 23 4)) 4) ; 5
(mod 5 8) ; 5

;; And this motivates the following recursion:

(defn ^:dynamic unpack [rl n]
  (if (empty? rl) '() 
      (let [[rh & rr] rl]
        (cons (mod n rh) (unpack rr (/ (- n (mod n rh)) rh))))))

;; Which behaves as I was hoping

(unpack '() 0) ;-> ()
(unpack '() 1) ;-> ()
(unpack '(8) 1) ;-> (1)
(unpack '(8) 9) ;-> (1)
(unpack '(8 8) 9) ;-> (1 1)

(unpack '(2 8 4 8) 380) ;-> (0 6 3 5)
(unpack '(2 8 4 8) 216) ;-> (0 4 1 3)
(unpack '(2 8 4 8) 168) ;-> (0 4 2 2)

;; And that then motivates the following packing function

(defn ^:dynamic pack [rlist vlist]
  (if (empty? rlist) 0
      (let [[rh & rr] rlist
            [vh & vr] vlist] 
        (+ (* rh (pack rr vr)) vh))))

;; Which also works
(pack '()()) ;-> 0
(pack '(8) '(1)) ;-> 1
(pack '(8 8) '(1 1)) ;-> 9
(pack '(2 8 4 8) '(0 6 3 5)) ;-> 380
(pack '(2 8 4 8) '(0 4 1 3)) ;-> 216
(pack '(2 8 4 8) '(0 4 2 2)) ;-> 168

;; But the cool thing is that it works whether or not the ranges are powers of two:

(use 'clojure.tools.trace)

(let [a (for [i (range (inc (rand-int 7)))] (rand-int 30))
      b (for [i a] (rand-int i))]
  (clojure.tools.trace/dotrace [unpack pack] (unpack a (pack a b))))

;; TRACE t3191: (pack (12 23 5 5 8 3) (4 9 3 4 0 0))
;; TRACE t3192: | (pack (23 5 5 8 3) (9 3 4 0 0))
;; TRACE t3193: | | (pack (5 5 8 3) (3 4 0 0))
;; TRACE t3194: | | | (pack (5 8 3) (4 0 0))
;; TRACE t3195: | | | | (pack (8 3) (0 0))
;; TRACE t3196: | | | | | (pack (3) (0))
;; TRACE t3197: | | | | | | (pack nil nil)
;; TRACE t3197: | | | | | | => 0
;; TRACE t3196: | | | | | => 0
;; TRACE t3195: | | | | => 0
;; TRACE t3194: | | | => 4
;; TRACE t3193: | | => 23
;; TRACE t3192: | => 538
;; TRACE t3191: => 6460
;; TRACE t3198: (unpack (12 23 5 5 8 3) 6460)
;; TRACE t3199: | (unpack (23 5 5 8 3) 538)
;; TRACE t3200: | | (unpack (5 5 8 3) 23)
;; TRACE t3201: | | | (unpack (5 8 3) 4)
;; TRACE t3202: | | | | (unpack (8 3) 0)
;; TRACE t3203: | | | | | (unpack (3) 0)
;; TRACE t3204: | | | | | | (unpack nil 0)
;; TRACE t3204: | | | | | | => ()
;; TRACE t3203: | | | | | => (0)
;; TRACE t3202: | | | | => (0 0)
;; TRACE t3201: | | | => (4 0 0)
;; TRACE t3200: | | => (3 4 0 0)
;; TRACE t3199: | => (9 3 4 0 0)
;; TRACE t3198: => (4 9 3 4 0 0)
;; (4 9 3 4 0 0)

;; Which I found slightly smug-making

;; So now we can say:
(defn deterministicfunction [big-rand]
  (let [[a b c d e] (unpack [2 100 1 100 100] big-rand)]
    (list 
     (case a 0 '+ 1 '-)
     b
     (list 
      (case c 0 '+ 1 '-)
      d
      e))))

(defn randomfunction []
  (deterministicfunction (rand-int (* 2 100 2 100 100))))

;; Behold the many benefits of this scheme:

;; We can enumerate all possible varieties
(deterministicfunction 1) ;-> (- 0 (+ 0 0))
(deterministicfunction 2) ;-> (+ 1 (+ 0 0))
(deterministicfunction 3) ;-> (- 1 (+ 0 0))

;; We can generate examples using a single number
(deterministicfunction 10345778979) ;-> (- 89 (+ 94 88))

;; Or we can get random examples
(randomfunction) ;-> (+ 99 (+ 72 8))

;; If we are not desperately fussed about the statistical properties of things
;; we do not need to worry about the exact range of the random number at all:
;; I think that this will produce only tiny biases:

(defn randomfunction []
  (deterministicfunction (rand-int (<< 1 30))))

;; And it still looks pretty random to me
(randomfunction) ;-> (+ 76 (+ 10 87))
(randomfunction) ;-> (+ 76 (+ 81 3))
(randomfunction) ;-> (- 27 (+ 18 63))





