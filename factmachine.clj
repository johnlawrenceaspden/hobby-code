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
  

(defn step [pc state controller]
  (if (>= pc (count controller)) :halt ;; if the program counter goes off the end, stop
      (let [npc (inc pc)                 ;; increment the program counter
            instruction (controller pc)] ;; look up the next instruction
        (cond
          ;; jump over labels
          (keyword? instruction) 
          [npc state controller]
          ;; assignment
          (= (first instruction) 'assign) 
          (let [var (second instruction)
                arg (nth instruction 2)]
            (cond (number? arg) 
                  [npc (assoc state var arg ) controller]
                  (symbol? arg)
                  [npc (assoc state var (state arg) ) controller]
                  :else
                  (let [[op val1 val2] arg]
                    [npc (assoc state var (operation op (state val1) (state val2))) controller])))
          ;; goto
          (= (first instruction) 'goto)
          [(.indexOf controller (second instruction)) state controller]
          ;; branch
          (= (first instruction) 'branch)
          (let [[op val1 val2] (second instruction)
                label (nth instruction 2)]
            (cond (= op '>) (if (operation op (state val1) (state val2))
                              [(.indexOf controller label) state controller]
                              [npc state controller])))))))


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









 
;; 
(def value      (ref 0)) ; #'user/value
(def n          (ref 0)) ; #'user/n
(def continue   (ref 0)) ; #'user/continue
(def stack      (ref (list))) ; #'user/stack
(defn dump [] {:value @value :n @n :continue @continue :stack @stack}) ; #'user/dump

(defn move [a b] (dosync (alter a (fn[_] @b)))(dump)) ; #'user/move
(defn assign [v n] (dosync (alter v (fn[_] n))) (dump)) ; #'user/assign
(defn fetch [v] @v) ; #'user/fetch
(defn goto [label] (str "GOTO " label)) ; #'user/goto
(defn save [x] (dosync (alter stack (fn[_] (cons @x _ )))) (dump)) ; #'user/save
(defn restore [v] (dosync (let [a (first @stack)] (alter v (fn[_] a)) (alter stack rest) a)) (dump)) ; #'user/restore
(defn is? [v n] (= @v n)) ; #'user/is?

(defn clearall [] (assign value 0) (assign n 0) (assign continue 'done) (assign stack '()) (dump)) ; #'user/clearall



(clearall)
(dump)
(assign value 10)
(dump)
(save value)
(dump)
(assign value 11)
(dump)
(move n value)
(dump)
(restore value)
(dump)

;; Calculating the factorial of three
(clearall) ; {:value 0, :n 0, :continue done, :stack ()}
(assign n 3) ; {:value 0, :n 3, :continue done, :stack ()}
;; BEGIN
(assign continue 'DONE) ; {:value 0, :n 3, :continue DONE, :stack ()}
;; LOOP 
(is? n 1) ; false
(save continue) ; {:value 0, :n 3, :continue DONE, :stack (DONE)}
(save n) ; {:value 0, :n 3, :continue DONE, :stack (3 DONE)}
(assign n (dec (fetch n))) ; {:value 0, :n 2, :continue DONE, :stack (3 DONE)}
(assign continue 'AFT) ; {:value 0, :n 2, :continue AFT, :stack (3 DONE)}
(goto 'LOOP) ; "GOTO LOOP"
(is? n 1) ; false
(save continue) ; {:value 0, :n 2, :continue AFT, :stack (AFT 3 DONE)}
(save n) ; {:value 0, :n 2, :continue AFT, :stack (2 AFT 3 DONE)}
(assign n (dec (fetch n))) ; {:value 0, :n 1, :continue AFT, :stack (2 AFT 3 DONE)}
(assign continue 'AFT) ; {:value 0, :n 1, :continue AFT, :stack (2 AFT 3 DONE)}
(goto 'LOOP) ; "GOTO LOOP"
(is? n 1) ; true
;;goto BASE
(assign value (fetch n)) ; {:value 1, :n 1, :continue AFT, :stack (2 AFT 3 DONE)}
(goto (fetch continue)) ; "GOTO AFT"
;; goto AFT
(restore n) ; {:value 1, :n 2, :continue AFT, :stack (AFT 3 DONE)}
(restore continue) ; {:value 1, :n 2, :continue AFT, :stack (3 DONE)}
(assign value (* (fetch n) (fetch value))) ; {:value 2, :n 2, :continue AFT, :stack (3 DONE)}
(fetch continue) ; AFT
;; goto AFT
(restore n) ; {:value 2, :n 3, :continue AFT, :stack (DONE)}
(restore continue) ; {:value 2, :n 3, :continue DONE, :stack ()}
(assign value (* (fetch n) (fetch value))) ; {:value 6, :n 3, :continue DONE, :stack ()}
(goto (fetch continue)) ; "GOTO DONE"
;; goto DONE
(fetch value) ; 6






