;; SICP Exercise 5.1

;; Design a register machine for the following algorithm

;; (define (factorial  n)
;;   (define (iter product counter)
;;     (if (> counter n)
;;         product
;;         (iter (* counter product)
;;               (+ counter 1))))
;;   (iter 1 1))

;; Or, in clojure

(defn factorial [n]
  (loop [product 1 counter 1]
    (if (> counter n)
      product
      (recur (* counter product) (inc counter)))))

(map factorial (range 10)) ; (1 1 2 6 24 120 720 5040 40320 362880)

;; We'll want registers for product and counter and n
(define-machine factorial
  (registers n product counter)
  (controller
   (assign product 1)
   (assign counter 1)
   :loop
   (branch (> counter n) :done)
   (assign product (* counter product))
   (assign counter (inc counter))
   (goto :loop)
   :done
   (assign n product)))

(def state
  '{n       0
    product 0
    counter 0})

(def controller
  '[(assign product 1)                    ;0
   (assign counter 1)                     ;1
   :loop                                  ;2
   (branch (> counter n) :done)           ;3
   (assign product (* counter product))   ;4
   (assign counter (inc counter))         ;5
   (goto :loop)                           ;6
   :done                                  ;7 
   (assign n product)])                   ;8


(defn step [pc state controller]
  (if (>= pc (count controller)) :halt
      (let [instruction (controller pc)]
        (cond
          (keyword? instruction)
          [(inc pc) state controller]
          (= (first instruction) 'assign)
          (let [pc (inc pc)
                var (second instruction)
                arg (nth instruction 2)]
            (cond (number? arg) 
                  [pc (assoc state var arg ) controller]
                  (symbol? arg)
                  [pc (assoc state var (state arg) ) controller]
                  (list arg)
                  (let [[op val1 val2] arg]
                    (cond (= op '*)
                          [pc
                           (assoc state var (* (state val1) (state val2)) )
                           controller]
                          (= op 'inc)
                          [pc
                           (assoc state var (inc (state val1)) )
                           controller]))))
          (= (first instruction) 'goto)
          [(.indexOf controller (second instruction)) state controller]
          (= (first instruction) 'branch)
          (let [[op val1 val2] (second instruction)
                label (nth instruction 2)]
            (cond (= op '>) (if (> (state val1) (state val2))
                              [(.indexOf controller label) state controller]
                              [(inc pc) state controller])))))))


(list 
 ;; labels and gotos
 (= (step 0 {} '[:begin (goto :begin)])
    '[1 {} [:begin (goto :begin)]]) ; true ; true ; true
 (= (step 1 {} '[:begin (goto :begin)]) '[0 {} [:begin (goto :begin)]]) ; true ; true ; true
 ;; assignment
 (= (step 0 {} '[(assign val 10)])
    '[1 {val 10} [(assign val 10)]]) ; true ; true ; true
 (= (step 0 '{doom 1} '[(assign val doom)])
    '[1 {val 1, doom 1} [(assign val doom)]]) ; true ; true
 (= (step 0 '{a 3 b 7} '[(assign val (* a b))])
    '[1 {val 21, a 3, b 7} [(assign val (* a b))]]) ; true ; true
 ;; branch
 (= (step 1 '{a 1 b 2} '[:begin (branch (> a b) :begin)]) '[2 {a 1, b 2} [:begin (branch (> a b) :begin)]]) ; true ; true ; true
 (= (step 1 '{a 2 b 1} '[:begin (branch (> a b) :begin)]) '[0 {a 2, b 1} [:begin (branch (> a b) :begin)]]) ; true ; true ; true
)


(step 0 state controller)
;; [1 {n 0, product 1, counter 0} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]])
(apply step (step 0 state controller))
;; [2 {n 0, product 1, counter 1} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
(apply step (apply step (step 0 state controller)))
;; [3 {n 0, product 1, counter 1} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]

(take-while #(not= :halt %) (iterate #(apply step %) [0 state controller]))
([0 {n 0, product 0, counter 0} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [1 {n 0, product 1, counter 0} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [2 {n 0, product 1, counter 1} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [3 {n 0, product 1, counter 1} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [7 {n 0, product 1, counter 1} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [8 {n 0, product 1, counter 1} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [9 {n 1, product 1, counter 1} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 )

(take-while #(not= :halt %) (iterate #(apply step %) [0 (assoc state 'n 3) controller]))
([0 {n 3, product 0, counter 0} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [1 {n 3, product 1, counter 0} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [2 {n 3, product 1, counter 1} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [3 {n 3, product 1, counter 1} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [4 {n 3, product 1, counter 1} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [5 {n 3, product 1, counter 1} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [6 {n 3, product 1, counter 2} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [2 {n 3, product 1, counter 2} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [3 {n 3, product 1, counter 2} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [4 {n 3, product 1, counter 2} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [5 {n 3, product 2, counter 2} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [6 {n 3, product 2, counter 3} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [2 {n 3, product 2, counter 3} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [3 {n 3, product 2, counter 3} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [4 {n 3, product 2, counter 3} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [5 {n 3, product 6, counter 3} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [6 {n 3, product 6, counter 4} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [2 {n 3, product 6, counter 4} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [3 {n 3, product 6, counter 4} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [7 {n 3, product 6, counter 4} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [8 {n 3, product 6, counter 4} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 [9 {n 6, product 6, counter 4} [(assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (assign product (* counter product)) (assign counter (inc counter)) (goto :loop) :done (assign n product)]]
 )


   
