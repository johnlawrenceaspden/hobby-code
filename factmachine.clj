;; A register machine for factorial

(defn fact [n]
  (if (= n 1)
    1
    (* n (fact (- n 1)))))

(fact 10) ; 3628800

;; We can't get away with just data paths and a finite-state controller for factorial
;; We also need a stack

 
;; In the data path we need
;; registers val and n
;; a way of putting the value of n into val
;; a way of telling if n is 1
;; a way of decrementing n
;; a way of multiplying n and val and putting the result into n

;; And we need a stack, let us say that it is twenty registers
;; There needs to be a way of putting n onto the stack

;; And we need a place to note states of the state machine,
;; This continue register needs to be able to store two states AFT and DONE

BEGIN
(assign continue done)
LOOP
(branch (= 1 (fetch n)) base)
(save continue)
(save n)
(assign n (dec (fetch n)))
(assign continue 'AFT)
(goto LOOP)
AFT
(restore n)
(restore continue)
(assign value (* (fetch n) (fetch value)))
(goto (fetch continue))
BASE
(assign value (fetch n))
(goto (fetch continue))
DONE


 
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






