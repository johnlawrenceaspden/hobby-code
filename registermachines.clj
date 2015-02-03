;; Register Machine Simulator

;; Which operations will we grant to our register machine?
(defn operation [[op aa bb] state]
  (let [ a (if (symbol? aa) (state aa) aa)
         b (if (symbol? bb) (state bb) bb)]
    (cond (= op '*) (* a b)
          (= op '+) (+ a b)
          (= op '-) (- a b)
          (= op '/) (quot a b)
          (= op 'mod) (mod a b)
          (= op '>) (> a b)
          (= op '>=) (>= a b)
          (= op '<) (< a b)
          (= op '<=) (<= a b)
          (= op 'inc) (inc a)
          (= op 'dec) (dec a)
          (= op '=) (= a b)
          :else :undefined)))

(defn ev [arg state]
  (cond
    ;; an immediate value
    (or (number? arg) (keyword? arg)) arg
    ;; a register name
    (symbol? arg) (state arg) 
    ;; an operation
    :else (operation arg state)))

;; Take a map representing the state of a machine,
;; e.g. {:pc 2 :state {n 1} :controller [:begin (goto :begin)]}
;; do the right thing to create the successor state
(defn step [{:keys [pc state controller stack] :as machine}]
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
                                      val (ev arg state) ]
                                  (assoc machine :pc npc :state (assoc state var val)))
                         
                         goto    (assoc machine :pc (.indexOf controller (if (symbol? s1) (state s1) s1)))
                         branch  (let [ test s1 label s2]
                                   (if (operation test state)
                                     (assoc machine :pc (.indexOf controller label))
                                     (assoc machine :pc npc)))
                         restore      (if (empty? stack) (assoc machine :pc :underflow)
                                          (assoc machine :pc npc :stack (rest stack) :state (assoc state s1 (first stack))))
                         save    (assoc machine :pc npc :stack (cons (state s1) stack))
                         (assoc machine :pc :error)))))))


(do
  (def basemachine {:pc 0  :state {} :controller '[] :dummy "hello" :stack '()})
  (def errmachine (assoc basemachine :controller '[(failzor)]))
  (def loopmachine (assoc basemachine :controller '[:begin (goto :begin)]))
  (def assignimmediatemachine  (assoc basemachine :controller '[(assign val 10)]))
  (def assignkeywordmachine    (assoc basemachine :controller '[(assign val :keyword)]))
  (def assignregistermachine1   (assoc basemachine :controller '[(assign val doom)]))
  (def assignregistermachine2   (assoc basemachine :controller '[(assign val doom)] :state '{doom 1 val 2}))
  (def assignoperationmachine   (assoc basemachine :controller '[(assign a (* a b))] :state '{a 3 b 5}))
  (def branchprogram '[(branch (> a b) :a>b) :a<=b :a>b])
  (def branchmachine    (assoc basemachine :state '{a 2 b 1} :controller branchprogram))
  (def nobranchmachine  (assoc basemachine :state '{a 1 b 2} :controller branchprogram))
  (def zerobranchmachine    {:state '{n 0}, :pc 0, :controller '[(branch (= 0 n) :zero) :nonzero :zero]})
  (def nonzerobranchmachine {:state '{n 1}, :pc 0, :controller '[(branch (= 0 n) :zero) :nonzero :zero]})
  (def savemachine    (assoc basemachine :state '{n 1} :controller '[(save n)]))
  (def restoremachine (assoc basemachine :stack '(1) :controller '[(restore n)]))
  (def oopsrestoremachine (assoc basemachine :stack '() :controller '[(restore n)]))
  (def swapmachine (assoc basemachine :state '{a :keyword b "string"} :controller '[(save a)(save b)(restore a)(restore b)]))
  (def gotocontinuemachine (assoc basemachine :controller '[:begin (assign continue :begin) (goto continue) :continue])))

(require 'clojure.data)
(defn check= [m1 m2]
  (let [[a b c] (clojure.data/diff m1 m2)]
    (if (and (nil? a)( nil? b))
      true
      [a b])))


(defn mtest[]
  (list
   ;; halting
   (check= (step basemachine) (assoc basemachine :pc :halt))
   (check= (step (step basemachine)) (step basemachine))
   ;; error state
   (check= (step errmachine) (assoc errmachine :pc :error))
   (check= (step (step errmachine)) (step errmachine))
   ;; labels and gotos
   (check= (step loopmachine) (assoc loopmachine :pc 1))
   (check= (step (step loopmachine)) loopmachine)
   (check= (step (step (step gotocontinuemachine))) (assoc gotocontinuemachine :state '{continue :begin}))
   ;; assignment
   (check= (step assignimmediatemachine) (assoc assignimmediatemachine :state '{val 10} :pc 1))
   (check= (step assignkeywordmachine) (assoc assignkeywordmachine :state '{val :keyword} :pc 1))
   (check= (step assignregistermachine1) (assoc assignregistermachine1 :state '{val nil} :pc 1))
   (check= (step assignregistermachine2) (assoc assignregistermachine2 :state '{val 1 doom 1} :pc 1))
   (check= (step assignoperationmachine) (assoc assignoperationmachine :state '{a 15 b 5} :pc 1))
   ;; branch
   (check= (step nobranchmachine)  (assoc nobranchmachine :pc 1))
   (check= (step branchmachine)    (assoc branchmachine :pc 2))
   (check= (step zerobranchmachine) (assoc zerobranchmachine :pc 2))
   (check= (step nonzerobranchmachine) (assoc nonzerobranchmachine :pc 1))
   ;; stack
   (check= (step savemachine)    (assoc savemachine :stack '(1) :pc 1))
   (check= (step restoremachine) (assoc restoremachine :state '{n 1} :stack '() :pc 1))
   (check= (step oopsrestoremachine) (assoc oopsrestoremachine :pc :underflow))
   (check= (step (step (step (step swapmachine)))) (assoc swapmachine :pc 4 :state '{b :keyword a "string"}))))

(mtest)





(def iterative-factorial-program
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
  { :pc 0 :controller controller :state (apply hash-map vars)})

(defn annotate [machine]
  (let [ann-machine
        ;; add a nexti field with the name of the instruction we're about to execute
        (assoc machine :nexti (#(get (:controller %) (:pc %)(:pc %)) machine))]
    ;; if the stack is empty nil it, to avoid the weird behaviour of print-table
    (if (empty? (ann-machine :stack)) (assoc ann-machine :stack nil) ann-machine)))

;; There appears to be a bug here in print-table, it says clojure.lang.PersistentList$EmptyList@1, instead of '()
(clojure.pprint/print-table (list {:c '()}))
(clojure.pprint/pprint { :c '()})

(defn annotated-run [maxiter machine]
  (let [m machine
        mseq (take maxiter (iterate step m))
        mrun (take (inc (count (take-while #(number? (:pc %)) mseq))) mseq)]
    (map annotate mrun)))

(clojure.pprint/print-table [:pc :state :nexti :stack ]
                            (annotated-run 100 (make-machine iterative-factorial-program 'n 0)))
;; |   :pc |                       :nexti |                      :state | :stack |
;; |-------+------------------------------+-----------------------------+--------|
;; |     0 |           (assign product 1) |                       {n 0} |        |
;; |     1 |           (assign counter 1) |            {n 0, product 1} |        |
;; |     2 |                        :loop | {n 0, product 1, counter 1} |        |
;; |     3 | (branch (> counter n) :done) | {n 0, product 1, counter 1} |        |
;; |     7 |                        :done | {n 0, product 1, counter 1} |        |
;; |     8 |           (assign n product) | {n 0, product 1, counter 1} |        |
;; |     9 |                            9 | {n 1, product 1, counter 1} |        |
;; | :halt |                        :halt | {n 1, product 1, counter 1} |        |


(defn factorial[n]
  (if (< n 2) 1 (* n (factorial (dec n)))))

(map factorial (range 22))
;; (1 1 2 6 24 120 720 5040 40320 362880
;; 3628800 39916800 479001600 6227020800 87178291200
;; 1307674368000 20922789888000 355687428096000 6402373705728000 121645100408832000
;; 2432902008176640000)

;; 20 factorial fills a 
(format "%X" 2432902008176640000) ; "21 C3 67 7C 82 B4 00 00" 
;; 21 factorial needs 66 bits
(clojure.pprint/cl-format nil "~X" 51090942171709440000) ; "2 c5 07 7d 36 b8 c4 00 00"

(def recursive-factorial-program
  '[:begin
    (assign continue :done)
    :loop
    (branch (= 0 n) :base)
    (save continue)
    (save n)
    (assign n (dec n))
    (assign continue :aft)
    (goto :loop)
    :aft
    (restore n)
    (restore continue)
    (assign value (* n value))
    (goto continue)
    :base
    (assign value 1)
    (goto continue)
    :done])


(clojure.pprint/print-table [:pc :state :nexti :stack ]
                            (annotated-run 1000 (make-machine recursive-factorial-program 'n 0)))

;; |   :pc |                  :nexti |                         :state | :stack |
;; |-------+-------------------------+--------------------------------+--------|
;; |     0 |                  :begin |                          {n 0} |        |
;; |     1 | (assign continue :done) |                          {n 0} |        |
;; |     2 |                   :loop |          {continue :done, n 0} |        |
;; |     3 |  (branch (= 0 n) :base) |          {continue :done, n 0} |        |
;; |    14 |                   :base |          {continue :done, n 0} |        |
;; |    15 |        (assign value 1) |          {continue :done, n 0} |        |
;; |    16 |         (goto continue) | {continue :done, value 1, n 0} |        |
;; |    17 |                   :done | {continue :done, value 1, n 0} |        |
;; |    18 |                      18 | {continue :done, value 1, n 0} |        |
;; | :halt |                   :halt | {continue :done, value 1, n 0} |        |

(clojure.pprint/print-table [:pc :state :nexti :stack ]
                            (annotated-run 1000 (make-machine recursive-factorial-program 'n 3)))

;; |   :pc |                         :state |                     :nexti |                  :stack |
;; |-------+--------------------------------+----------------------------+-------------------------|
;; |     0 |                          {n 3} |                     :begin |                         |
;; |     1 |                          {n 3} |    (assign continue :done) |                         |
;; |     2 |          {continue :done, n 3} |                      :loop |                         |
;; |     3 |          {continue :done, n 3} |     (branch (= 0 n) :base) |                         |
;; |     4 |          {continue :done, n 3} |            (save continue) |                         |
;; |     5 |          {continue :done, n 3} |                   (save n) |                 (:done) |
;; |     6 |          {continue :done, n 3} |         (assign n (dec n)) |               (3 :done) |
;; |     7 |          {continue :done, n 2} |     (assign continue :aft) |               (3 :done) |
;; |     8 |           {continue :aft, n 2} |               (goto :loop) |               (3 :done) |
;; |     2 |           {continue :aft, n 2} |                      :loop |               (3 :done) |
;; |     3 |           {continue :aft, n 2} |     (branch (= 0 n) :base) |               (3 :done) |
;; |     4 |           {continue :aft, n 2} |            (save continue) |               (3 :done) |
;; |     5 |           {continue :aft, n 2} |                   (save n) |          (:aft 3 :done) |
;; |     6 |           {continue :aft, n 2} |         (assign n (dec n)) |        (2 :aft 3 :done) |
;; |     7 |           {continue :aft, n 1} |     (assign continue :aft) |        (2 :aft 3 :done) |
;; |     8 |           {continue :aft, n 1} |               (goto :loop) |        (2 :aft 3 :done) |
;; |     2 |           {continue :aft, n 1} |                      :loop |        (2 :aft 3 :done) |
;; |     3 |           {continue :aft, n 1} |     (branch (= 0 n) :base) |        (2 :aft 3 :done) |
;; |     4 |           {continue :aft, n 1} |            (save continue) |        (2 :aft 3 :done) |
;; |     5 |           {continue :aft, n 1} |                   (save n) |   (:aft 2 :aft 3 :done) |
;; |     6 |           {continue :aft, n 1} |         (assign n (dec n)) | (1 :aft 2 :aft 3 :done) |
;; |     7 |           {continue :aft, n 0} |     (assign continue :aft) | (1 :aft 2 :aft 3 :done) |
;; |     8 |           {continue :aft, n 0} |               (goto :loop) | (1 :aft 2 :aft 3 :done) |
;; |     2 |           {continue :aft, n 0} |                      :loop | (1 :aft 2 :aft 3 :done) |
;; |     3 |           {continue :aft, n 0} |     (branch (= 0 n) :base) | (1 :aft 2 :aft 3 :done) |
;; |    14 |           {continue :aft, n 0} |                      :base | (1 :aft 2 :aft 3 :done) |
;; |    15 |           {continue :aft, n 0} |           (assign value 1) | (1 :aft 2 :aft 3 :done) |
;; |    16 |  {continue :aft, value 1, n 0} |            (goto continue) | (1 :aft 2 :aft 3 :done) |
;; |     9 |  {continue :aft, value 1, n 0} |                       :aft | (1 :aft 2 :aft 3 :done) |
;; |    10 |  {continue :aft, value 1, n 0} |                (restore n) | (1 :aft 2 :aft 3 :done) |
;; |    11 |  {continue :aft, value 1, n 1} |         (restore continue) |   (:aft 2 :aft 3 :done) |
;; |    12 |  {continue :aft, value 1, n 1} | (assign value (* n value)) |        (2 :aft 3 :done) |
;; |    13 |  {continue :aft, value 1, n 1} |            (goto continue) |        (2 :aft 3 :done) |
;; |     9 |  {continue :aft, value 1, n 1} |                       :aft |        (2 :aft 3 :done) |
;; |    10 |  {continue :aft, value 1, n 1} |                (restore n) |        (2 :aft 3 :done) |
;; |    11 |  {continue :aft, value 1, n 2} |         (restore continue) |          (:aft 3 :done) |
;; |    12 |  {continue :aft, value 1, n 2} | (assign value (* n value)) |               (3 :done) |
;; |    13 |  {continue :aft, value 2, n 2} |            (goto continue) |               (3 :done) |
;; |     9 |  {continue :aft, value 2, n 2} |                       :aft |               (3 :done) |
;; |    10 |  {continue :aft, value 2, n 2} |                (restore n) |               (3 :done) |
;; |    11 |  {continue :aft, value 2, n 3} |         (restore continue) |                 (:done) |
;; |    12 | {continue :done, value 2, n 3} | (assign value (* n value)) |                         |
;; |    13 | {continue :done, value 6, n 3} |            (goto continue) |                         |
;; |    17 | {continue :done, value 6, n 3} |                      :done |                         |
;; |    18 | {continue :done, value 6, n 3} |                         18 |                         |
;; | :halt | {continue :done, value 6, n 3} |                      :halt |                         |



(def recursive-fibonacci-program
  '[:begin
    (assign continue :done)
    :loop
    (branch (= 0 n) :base)
    (save continue)
    (save n)
    (assign n (dec n))
    (assign continue :aft)
    (goto :loop)
    :aft
    (restore n)
    (restore continue)
    (assign value (* n value))
    (goto continue)
    :base
    (assign value 1)
    (goto continue)
    :done])

(defn fib[n]
  (if (< n 2) n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(map fib (range 10)) ; (0 1 1 2 3 5 8 13 21 34)

;; A pseudocody version
;; to fib(n):
;;   is n < 2?
;;   if not
;;   return n
;;   else
;;   n1=n-1
;;   n2=n-2
;;   sum = call fib(n1)
;;   sum += call fib(n2)
;;   return sum

;; call fib(3)

;; Now in made-up assembler
(def recursive-fibonacci-program
  '[:begin
    (assign continue :done)
    (goto :fib)
    ;; answer now in value
    :done
    (goto :end)
    :fib ;; n contains parameter
    (branch (>= n 2) :recurse-fib)
    (assign value n)
    (goto continue)
    :recurse-fib
    (save continue)
    (save n)
    (assign n (- n 1))
    (assign continue :after-fib-1)
    (goto :fib)
    ;; answer now in value
    :after-fib-1
    (restore n)
    (restore continue)
    (assign sum value)
    (save continue)
    (save n)
    (save sum)
    (assign n (- n 2))
    (assign continue :after-fib-2)
    (goto :fib)
    :after-fib-2
    (restore sum)
    (restore n)
    (restore continue)
    (assign sum (+ sum value))
    (assign value sum)
    (goto continue)
    :end
    ])

(clojure.pprint/print-table
 [:pc  :stack :state :nexti ]
 (annotated-run 1000 (make-machine recursive-fibonacci-program 'n 6)))

(count (annotated-run 1000 (make-machine recursive-fibonacci-program 'n 0))) ; 12
(count (annotated-run 1000 (make-machine recursive-fibonacci-program 'n 1))) ; 12
(count (annotated-run 1000 (make-machine recursive-fibonacci-program 'n 2))) ; 41
(count (annotated-run 1000 (make-machine recursive-fibonacci-program 'n 3))) ; 70
(count (annotated-run 1000 (make-machine recursive-fibonacci-program 'n 4))) ; 128
(count (annotated-run 1000 (make-machine recursive-fibonacci-program 'n 5))) ; 215
(count (annotated-run 1000 (make-machine recursive-fibonacci-program 'n 6))) ; 360

(- 41  12 12)   ; 17
(- 70  41 12)   ; 17
(- 128 70 41)   ; 17
(- 215 128 70)  ; 17
(- 360 215 128) ; 17

(defn fibtime [n]
  (if (< n 2) 12
      (+ 17 (fibtime (- n 1)) (fibtime (- n 2)))))

(map fib (range 10)) ; (1 1 2 3 5 8 13 21 34)     ; (0 1 1 2 3 5 8 13 21 34)
(map fibtime (range 10)) ; (12 41 70 128 215 360 592 969 1578) ; (12 12 41 70 128 215 360 592 969 1578)

(map / 
     (map fib (range 10)) 
     (map fibtime (range 10))) ; (0 1/12 1/41 1/35 3/128 1/43 1/45 13/592 7/323 17/789)

(map float
     (map / 
          (map fib (range 10)) 
          (map fibtime (range 10)))) ; (0.0 0.083333336 0.024390243 0.028571429 0.0234375 0.023255814 0.022222223 0.02195946 0.021671826 0.021546261)


(def run6 (annotated-run 1000 (make-machine recursive-fibonacci-program 'n 6)))

;; profiling
(defn profile [run]
  (let [program ((first run) :controller)
        lf (frequencies (map :pc run))]  
    (doseq [i (range (count program))]
      (println (format "%6d" (lf i)) (recursive-fibonacci-program i) ))))

(profile run6)

;; Now in made-up assembler
(def tweaked-fibonacci-program
  '[:begin
    (assign continue :done)
    (goto :fib)
    ;; answer now in value
    :done
    (goto :end)
    :fib ;; n contains parameter
    (branch (>= n 2) :recurse-fib)
    (assign value n)
    (goto continue)
    :recurse-fib
    (save continue)
    (save n)
    (assign n (- n 1))
    (assign continue :after-fib-1)
    (goto :fib)
    ;; answer now in value
    :after-fib-1
    (restore n)
    (save n)
    (assign sum value)
    (save sum)

    ; by reordering, we can make these three instructions adjacent, and see that we can remove two of them
    ;(restore continue)
    ;(save continue)
    (assign continue :after-fib-2)

    (assign n (- n 2))

    (goto :fib)
    :after-fib-2
    (restore sum)
    (restore n)
    (restore continue)
    (assign sum (+ sum value))
    (assign value sum)
    (goto continue)
    :end
    ])

(let [r6 (annotated-run 1000 (make-machine recursive-fibonacci-program 'n 6))]
  [(count r6), ('value (:state (last run6)))]) ; [360 8]

(let [r6 (annotated-run 1000 (make-machine tweaked-fibonacci-program 'n 6))]
  [(count r6), ('value (:state (last run6)))]) ; [336 8]


(apply max (map count (map :stack (annotated-run 1000 (make-machine tweaked-fibonacci-program 'n 6))))) ; 11
(apply max (map count (map :stack (annotated-run 1000 (make-machine recursive-fibonacci-program 'n 6))))) ; 11
