;; Register Machine Simulator

;; Which operations will we grant to our register machine?
(defn operation [[op aa bb] state]
  (let [ a (if (symbol? aa) (state aa) aa)
         b (if (symbol? bb) (state bb) bb)]
         
    (cond (= op '*) (* a b)
          (= op '>) (> a b)
          (= op 'inc) (inc a))))

;; Take a map representing the state of a machine,
;; e.g. {:pc 2 :state {n 1} :controller [:begin (goto :begin)]}
;; do the right thing to create the successor state
(operation op (state val1) (state val2))
(operation op (state val1) (state val2))

(do
  (defn step [{:keys [pc state controller] :as machine}]
    (print pc)
    ;; if the program counter is not a number, do nothing
    (cond (not (number? pc)) machine 
          (>= pc (count controller)) (assoc machine :pc :halt) ;; if the program counter goes off the end, stop
          :else  (let [npc (inc pc)                 ;; increment the program counter
                       instruction (controller pc)] ;; look up the next instruction
                   (if (keyword? instruction) (assoc machine :pc npc)
                       (let [[opcode s1 s2] instruction]
                         (case opcode
                           assign (let [ var s1 arg s2
                                         val (cond
                                               ;; immediate values
                                               (or (number? arg) (keyword? arg)) arg
                                               ;; registers
                                               (symbol? arg) (state arg) 
                                               ;; operations on registers
                                               :else (operation arg state))]
                                     (assoc machine :pc npc :state (assoc state var val)))
                     
                           goto    (assoc machine :pc (.indexOf controller s1))
                           branch  (let [ test s1 label s2]
                                      (if (operation test state)
                                        (assoc machine :pc (.indexOf controller label))
                                        (assoc machine :pc npc)))
                           (assoc machine :pc :error)))))))
  (mtest))


(def basemachine {:pc 0  :state {} :controller '[]})
(def errmachine (assoc basemachine :controller '[(failzor)]))
(def loopmachine (assoc basemachine :controller '[:begin (goto :begin)]))
(def assignmachine (assoc basemachine :controller '[(assign val 10)]))
(def branchprogram '[(branch (> a b) :a>b) :a<=b :a>b])
(def branchmachine {:pc 0 :state '{a 2 b 1} :controller branchprogram })
(def nobranchmachine  {:pc 0 :state '{a 1 b 2} :controller branchprogram})


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
   (= (step nobranchmachine)  (assoc nobranchmachine :pc 1))
   (= (step branchmachine)    (assoc branchmachine :pc 2))))

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
