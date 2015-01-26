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
  '[:begin
    (assign product 1)
    (assign counter 1)
    :loop
    (branch (> counter n) :done)
    (goto :begin)
    :done])


(defn step [pc state controller]
  (let [instruction (controller pc)]
    (cond
      (keyword? instruction)
      [(inc pc) state controller]
      (= (first instruction) 'assign)
      (let [pc (inc pc)]
        [pc (assoc state (second instruction) (nth instruction 2)) controller])
      (= (first instruction) 'goto)
        [(.indexOf controller :begin) state controller]
        (= (first instruction) 'branch)
        (let [[op val1 val2] (second instruction)
              label (nth instruction 2)]
          (cond (= op '>) (if (> (state val1) (state val2))
                            [(.indexOf controller label) state controller]
                            [(inc pc) state controller]))))))

;; labels and gotos
(= (step 0 {} '[:begin (goto :begin)]) '[1 {} [:begin (goto :begin)]]) ; true ; true
(= (step 1 {} '[:begin (goto :begin)]) '[0 {} [:begin (goto :begin)]]) ; true ; true
;; assignment
(= (step 0 {} '[(assign val 10)])' [1 {val 10} [(assign val 10)]]) ; true
;; branch
(= (step 1 '{a 1 b 2} '[:begin (branch (> a b) :begin)]) '[2 {a 1, b 2} [:begin (branch (> a b) :begin)]]) ; true ; true
(= (step 1 '{a 2 b 1} '[:begin (branch (> a b) :begin)]) '[0 {a 2, b 1} [:begin (branch (> a b) :begin)]]) ; true ; true



(step 0 state controller) ; [1 {n 0, product 0, counter 0} [:begin (assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (goto :begin) :done]]
(step 1 state controller) ; [2 {n 0, product 1, counter 0} [:begin (assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (goto :begin) :done]]
(step 2 state controller) ; [3 {n 0, product 0, counter 1} [:begin (assign product 1) (assign counter 1) :loop (branch (> counter n) :done) (goto :begin) :done]]

   
