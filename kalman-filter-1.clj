;; Kalman Filtering I - How to take an average

;; Use scheme-like defines
(defmacro define [var expr] 
  (cond (symbol? var)  `(let [tmp# ~expr] (def ~var tmp#) tmp#)
        (list? var)  `(defn ~(first var) ~(into [] (rest var)) ~expr)))


;; Imagine you've got a car battery, which has been sitting in a cupboard for a few months.

;; It's in a nice stable state, and will contain some charge. 

;; The voltage is a real thing. Imagine it's 12.4 volts exactly, meaning that your battery's probably about 65% charged.

(define voltage 12.4) ;-> 12.4

;; But you're measuring the voltage with a dodgy voltmeter, which gives a different answer every time you try it.

(define (random-error)
  (+ (rand) -0.5)) ;-> #'user/random-error

(random-error) ;-> 0.08195513179741798


(define (get-reading)
  (+ voltage (random-error)(random-error)(random-error))) ;-> #'user/get-reading

(get-reading) ;-> 12.107575994921698
(get-reading) ;-> 11.797335826782009
(get-reading) ;-> 12.667462812960427
(get-reading) ;-> 12.576862655678447
(get-reading) ;-> 12.84286181019528

;; One way to get the real answer would be to take a lot of readings and to take their average.



(define readings (repeatedly get-reading)) ;-> (12.228781558868684 12.472775922959098 12.885678616139417 12.56918012763732 12.809164590415573 11.932143652998937 11.914847023266017 12.022511173224883 12.288182086409893 12.779379265443911 11.857603896477945 12.378224320281287 13.129424186135443 12.469816020716848 12.102492132981764 12.781507010631653 13.238839759845604 13.615776288751464 12.066974867461743 12.963791036127024 12.112703324217074 12.335140800542293 11.69208189450539 12.862809688032863 12.544152115246746 11.602037996791525 13.537450025671546 ...)

(/ (reduce + (take 3 readings)) 3) ;-> 12.5290786993224

(/ (reduce + (take 10 readings)) 10) ;-> 12.390264401736372

(/ (reduce + (take 30 readings)) 30) ;-> 12.485162247628404

(/ (reduce + (take 100 readings)) 100) ;-> 12.457517452194962

(/ (reduce + (take 300 readings)) 300) ;-> 12.424751798077644

;; As you can see, the more readings that you have, the less accurate your estimate will be....


;; On the other hand, if we imagine that our readings are being taken one-by-one, we can imagine updating the best guess as the estimates come in

(define (average n sq)
  (/ (reduce + (take n sq)) n)) ;-> #'user/average

(define moving-average (map #(average % readings) (iterate inc 1))) ;-> (12.228781558868684 12.350778740913892 12.5290786993224 12.53910405640113 12.59311616320402 12.482954078169838 12.401795927469292 12.354385333188741 12.34702941687998 12.390264401736372 12.341840719440153 12.344872686176913 12.40522280155834 12.409836602926804 12.389346971597133 12.413856974036792 12.462385373202016 12.526462646288097 12.502279078981447 12.525354676838726 12.50570461242817 12.497951711887906 12.46291389374084 12.479576218503007 12.482159254372757 12.448308436773479 12.488647014140074 ...)

;; Of course, recalculating every time a new reading comes in involves
;; keeping a lot of old readings around, and re-does a lot of
;; redundant computations, so how about keeping a count of how many
;; readings we've already seen, and using that to recalculate our new
;; average as the readings come in?

(define (average-filter [avg k] new-reading)
  [(/ (+ new-reading (* k avg)) (inc k)) (inc k)]) ;-> #'user/average-filter

; Now we can work out our average in stages. Taking the sequence 1 2 1 2 1 2 1 2 ..
(average-filter [0 0] 1) ;-> [1 1]
(average-filter [1 1] 2) ;-> [3/2 2]
(average-filter [3/2 2] 1) ;-> [4/3 3]
(average-filter [4/3 3] 2) ;-> [3/2 4]

;; or
(-> [0 0]
    (average-filter 1)
    (average-filter 2)
    (average-filter 1)
    (average-filter 2)
    (average-filter 1)
    (average-filter 2)) ;-> [3/2 6]

;; or 

(reductions average-filter [0 0] '(1 2 1 2 1 2)) ;-> ([0 0] [1 1] [3/2 2] [4/3 3] [3/2 4] [7/5 5] [3/2 6])

;; or

(map first (reductions average-filter [0 0] '(1 2 1 2 1 2))) ;-> (0 1 3/2 4/3 3/2 7/5 3/2)

;; So that if we want the running averages of our battery readings:

(map first (reductions average-filter [0 0] readings)) ;-> (0 12.228781558868684 12.350778740913892 12.5290786993224 12.53910405640113 12.59311616320402 12.482954078169838 12.401795927469292 12.354385333188741 12.34702941687998 12.390264401736372 12.341840719440153 12.344872686176913 12.40522280155834 12.409836602926804 12.389346971597133 12.413856974036792 12.462385373202016 12.526462646288097 12.502279078981447 12.525354676838726 12.50570461242817 12.497951711887906 12.46291389374084 12.479576218503007 12.482159254372757 12.448308436773479 ...)

(define (average-sequence sq) 
  (map first (reductions average-filter [0 0] sq))) ;-> #'user/average-sequence

(average-sequence readings) ;-> (0 12.228781558868684 12.350778740913892 12.5290786993224 12.53910405640113 12.59311616320402 12.482954078169838 12.401795927469292 12.354385333188741 12.34702941687998 12.390264401736372 12.341840719440153 12.344872686176913 12.40522280155834 12.409836602926804 12.389346971597133 12.413856974036792 12.462385373202016 12.526462646288097 12.502279078981447 12.525354676838726 12.50570461242817 12.497951711887906 12.46291389374084 12.479576218503007 12.482159254372757 12.448308436773479 ...)




