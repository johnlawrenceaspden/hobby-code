;; A register machine for factorial

(defn fact [n]
  (if (= n 1)
    1
    (* n (fact (- n 1)))))

(fact 10) ; 3628800

;; We can't get away with just data paths and a finite-state controller for factorial
;; We also need a stack

 
;; In the data path we need
;; registers value and n
;; a way of putting the value of n into value
;; a way of telling if n is 1
;; a way of decrementing n
;; a way of multiplying n and value and putting the result into n

;; And we need a stack, let us say that it is twenty registers
;; There needs to be a way of putting n onto the stack

;; And we need a place to note states of the state machine,
;; This continue register needs to be able to store two states AFT and DONE

:begin
(assign continue :done)
:loop
(branch (= 1 (fetch n)) :base)
(save continue)
(save n)
(assign n (dec (fetch n)))
(assign continue :aft)
(goto :loop)
:aft
(restore n)
(restore continue)
(assign value (* (fetch n) (fetch value)))
(goto (fetch continue))
:base
(assign value (fetch n))
(goto (fetch continue))
:done

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn operation [op a b]
  (cond (= op '*) (* a b)
        (= op '>) (> a b)
        (= op 'inc) (inc a)))
  

(defn step [{:keys [pc state controller] :as machine}]
  (print pc)
  (if (keyword? pc) machine
      (if (>= pc (count controller)) (assoc machine :pc :halt) ;; if the program counter goes off the end, just cycle
          (let [npc (inc pc)                 ;; increment the program counter
                instruction (controller pc)] ;; look up the next instruction
            (cond
              ;; jump over labels
              (keyword? instruction) 
              (assoc machine :pc npc)
              ;; assignment
              (= (first instruction) 'assign) 
              (let [var (second instruction)
                    arg (nth instruction 2)
                    val (cond
                          ;; immediate values
                          (or (number? arg) (keyword? arg)) arg
                          ;; registers
                          (symbol? arg) (state arg) 
                          ;; operations on registers
                          :else
                          (let [[op val1 val2] arg]
                            (operation op (state val1) (state val2))))]
                (assoc machine :pc npc :state (assoc state var val )))
              ;; goto
              (= (first instruction) 'goto)
              {:pc (.indexOf controller (second instruction)) :state state :controller controller}
              ;; branch
              (= (first instruction) 'branch)
              (let [[op val1 val2] (second instruction)
                    label (nth instruction 2)]
                (if (operation op (state val1) (state val2))
                  (assoc machine :pc (.indexOf controller label))
                  (assoc machine :pc npc)))
              :else
              (assoc machine :pc :error))))))


(def basemachine {:pc 0  :state {} :controller '[]})
(def errmachine (assoc basemachine :controller '[(failzor)]))
(def loopmachine (assoc basemachine :controller '[:begin (goto :begin)]))
(def assignmachine (assoc basemachine :controller '[(assign val 10)]))

(defn mtest[]
  (list
   ;; halting
   (= (step basemachine) (assoc basemachine :pc :halt))
   (= (step (step basemachine)) (step basemachine))
   ;; error state
   (= (step errmachine) (assoc errmachine :pc :error))
   (= (step (step errmachine)) (step errmachine))
   ;; labels and gotos
   (= (step loopmachine) (assoc loopmachine :pc 1))
   (= (step (step loopmachine)) loopmachine)
   ;; assignment
   (= (step assignmachine)
      (assoc assignmachine
             :state '{val 10}
             :pc 1))
   (= (step {:pc 0 :state '{} :controller '[(assign val :keyword)]})
      {:pc 1, :state '{val :keyword}, :controller '[(assign val :keyword)]})
   (= (step {:pc 0 :state '{doom 1} :controller '[(assign val doom)]})
      '{:pc 1, :state {val 1, doom 1}, :controller [(assign val doom)]})
   (= (step {:pc 0 :state '{a 3 b 7} :controller '[(assign val (* a b))]})
      '{:pc 1, :state {val 21, a 3, b 7}, :controller [(assign val (* a b))]})
   ;; branch
   (= (step {:pc 1 :state '{a 1 b 2} :controller '[:begin (branch (> a b) :begin)]}) '{:pc 2, :state {a 1, b 2}, :controller [:begin (branch (> a b) :begin)]})
   (= (step {:pc 1 :state '{a 2 b 1} :controller '[:begin (branch (> a b) :begin)]}) '{:pc 0, :state {a 2, b 1}, :controller [:begin (branch (> a b) :begin)]})))

(def iterative-factorial
  '[(assign product 1)                     ;0
    (assign counter 1)                     ;1
    :loop                                  ;2
    (branch (> counter n) :done)           ;3
    (assign product (* counter product))   ;4
    (assign counter (inc counter))         ;5
    (goto :loop)                           ;6
    :done                                  ;7 
    (assign n product)                     ;8
    ])

(defn make-machine [controller & vars]
  { :controller controller :vars (apply hash-map vars)})

(def ifrun (iterate step (make-machine iterative-factorial 'n 0)))

(def ifrun (iterate step {:state '{n 0} :pc 0 :controller iterative-factorial}))

(map :pc ifrun) ; (0 1 2 3 7 8 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 ...)
(take 8 (map (juxt #(get (:controller %) (:pc %)(:pc %) ) :state) ifrun)) ;



(def recursive-factorial
  '[:begin
    (assign continue :done)
    :loop
    (branch (= 1 (fetch n)) :base)
    (save continue)
    (save n)
    (assign n (dec (fetch n)))
    (assign continue :aft)
    (goto :loop)
    :aft
    (restore n)
    (restore continue)
    (assign value (* (fetch n) (fetch value)))
    (goto (fetch continue))
    :base
    (assign value (fetch n))
    (goto (fetch continue))
    :done])


(def rfm {:state '{n 0} :pc 0 :controller recursive-factorial})
(def rfmseq (take 1000 (iterate step rfm)))
(def rfrun (take (inc (count (take-while #(number? (:pc %)) rfmseq))) rfmseq))

(map :pc rfrun) ; (0 1 2 3 4 :error)

(clojure.pprint/print-table [:pc :state] rfrun)
