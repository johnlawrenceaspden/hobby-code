;; Finite Automata

;; I just did Jeff Ullmann's Stanford Course 'Automata' through Coursera. It was fascinating.

;; A finite automaton is a mathematical model representing a simple computer. But it's very close to being a computer.
;; In fact you can code them up in Verilog and make them out of silicon. In chip design they're known as finite state machines.

;; They're strictly less powerful than real computers and programs, but they can still do lots of stuff.
;; On the other hand, we can determine what it is that they do much more easily than we can with computers and programs.

;; Here's an example of an automaton

;; Start at A
;; A -0-> A
;; A -1-> B
;; B -1-> B
;; B -0-> A
;; B is accepting

;; INSERT IMAGE automaton1.png

(def automatonI {:start :A
                 :accept #{:B}
                 :transition { 1 {:A :B :B :B}
                               0 {:A :A :B :A}}})

;; How should we work out whether this automaton accepts the string 101101011 ? 

;; It starts in state :A
(automatonI :start) ;-> :A

;; The first character of the string is a 1, and so it transitions to B
(((automatonI :transition) 1) (automatonI :start)) ;-> :B

;; The second character is 0, so it transitions back to :A
(((automatonI :transition) 0) (((automatonI :transition) 1) (automatonI :start))) ;-> :A

;; And so on
(reduce (fn[state input] (((automatonI :transition) input) state)) (automatonI :start) (list 1 0 1 1 0 1 0 1 1)) ;-> :B

;; The final state after all the input is consumed is :B, which is an accepting state

(defn final-state [automaton string]
  (reduce (fn[state input] 
            (((automaton :transition) input) state)) 
          (automaton :start) 
          string))


(final-state automatonI (list 1 0 1 1 0 1 0 1 1)) ;-> :B

;; And so the string 101101011 is accepted by the automaton.

(defn accepts [automaton string]
  (not (nil? ((automaton :accept) 
              (final-state automaton string))))

;; We might ask what other types of strings are accepted

(defn extend [ss] (for [a ss b '(0 1)] (cons b a)))

(extend '(())) ;-> ((0) (1))
(extend (extend '(()))) ;-> ((0 0) (1 0) (0 1) (1 1))

;; All these strings together are rather charmingly known as the free monoid on #{0,1} 
;; But enough of that! We know what strings are!

(def strings01 (apply concat (iterate extend '(()))))


strings01 ;-> (() (0) (1) (0 0) (1 0) (0 1) (1 1) (0 0 0) (1 0 0) (0 1 0) (1 1 0) (0 0 1) (1 0 1) (0 1 1) (1 1 1) (0 0 0 0) (1 0 0 0) (0 1 0 0) (1 1 0 0) (0 0 1 0) (1 0 1 0) (0 1 1 0) (1 1 1 0) (0 0 0 1) (1 0 0 1) (0 1 0 1) (1 1 0 1) ...)

(filter (partial accepts automatonI) strings01) ;-> ((1) (0 1) (1 1) (0 0 1) (1 0 1) (0 1 1) (1 1 1) (0 0 0 1) (1 0 0 1) (0 1 0 1) (1 1 0 1) (0 0 1 1) (1 0 1 1) (0 1 1 1) (1 1 1 1) (0 0 0 0 1) (1 0 0 0 1) (0 1 0 0 1) (1 1 0 0 1) (0 0 1 0 1) (1 0 1 0 1) (0 1 1 0 1) (1 1 1 0 1) (0 0 0 1 1) (1 0 0 1 1) (0 1 0 1 1) (1 1 0 1 1) ...)

;; As you may have guessed, this automaton accepts only those strings that end in 1

(map last (filter (partial accepts automatonI) strings01)) ;-> (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ...)
(map last (filter (comp not (partial accepts automatonI)) strings01)) ;-> (nil 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ...)

;; Here's a slightly more interesting one:

;; INSERT AUTOMATONII.PNG

(def automatonII {:start :A
                  :accept #{:B}
                  :transition { 1 {:A :B :B :A}
                                0 {:A :A :B :B}}})


;; Can you see what it is doing?
(filter (partial accepts automatonII) strings01) ;-> ((1) (1 0) (0 1) (1 0 0) (0 1 0) (0 0 1) (1 1 1) (1 0 0 0) (0 1 0 0) (0 0 1 0) (1 1 1 0) (0 0 0 1) (1 1 0 1) (1 0 1 1) (0 1 1 1) (1 0 0 0 0) (0 1 0 0 0) (0 0 1 0 0) (1 1 1 0 0) (0 0 0 1 0) (1 1 0 1 0) (1 0 1 1 0) (0 1 1 1 0) (0 0 0 0 1) (1 1 0 0 1) (1 0 1 0 1) (0 1 1 0 1) ...)

;; And finally:
(def automatonIII {:start :A
                   :accept #{:A}
                   :transition { 1 {:A :B :B :A :C :C}
                                 0 {:A :A :B :C :C :B}}})

(filter (partial accepts automatonIII) strings01) ;-> (() (0) (0 0) (1 1) (0 0 0) (1 1 0) (0 1 1) (0 0 0 0) (1 1 0 0) (0 1 1 0) (1 0 0 1) (0 0 1 1) (1 1 1 1) (0 0 0 0 0) (1 1 0 0 0) (0 1 1 0 0) (1 0 0 1 0) (0 0 1 1 0) (1 1 1 1 0) (0 1 0 0 1) (1 0 1 0 1) (0 0 0 1 1) (1 1 0 1 1) (0 1 1 1 1) (0 0 0 0 0 0) (1 1 0 0 0 0) (0 1 1 0 0 0) ...)

;; Can you see what it's doing? 


;; Here's a clue

(defn to-integer [lst]
  (cond (empty? lst) 0
        (= (first lst) 1) (+ 1 (* 2 (to-integer (rest lst))))
        :else (* 2 (to-integer (rest lst)))))

(map to-integer strings01)

(map to-integer (filter (partial accepts automatonIII) strings01)) ;-> (0 0 0 3 0 3 6 0 3 6 9 12 13 15 0 3 6 9 12 13 15 18 24 26 27 30 0 ...)

(for [[k v] (group-by (partial accepts automatonIII) (take 100 strings01))]
  [k (sort (distinct (map to-integer v)))])

;; Can you get it to work the right way round? 

;; Can you generalize it?
