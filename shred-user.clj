;; I often find myself restarting the clojure repl in order to get rid of old
;; definitions which introduce subtle bugs.

;; Consider:

(defn factorial [n] (if (< n 2) 1 (* n (factorial (dec n))))) ; #'user/factorial

(factorial 10) ; 3628800

;; Suppose instead I wanted (factorial2 n) to be (* 1 2 2 2 3 2 4 .... 2 n)

(defn factorial2 [n] (if (< n 2) 1 (* 2 n (factorial (dec n))))) ; #'user/factorial2

;; There is a subtle bug introduced by my failing to rename the recursive call

(map factorial2 '(1 2 3 4)) ; (1 4 24 192)

;; But (factorial2 4) should be (* 1 2 2 2 3 2 4) = 192

;; I suspect that there is an old definition hanging around confusing matters.

;; To find it, I can run the program from the command line, or restart the repl and recompile, or:

(doseq [s (map first (ns-interns 'user))](ns-unmap 'user s)) ; nil

(defn factorial2 [n] (if (< n 2) 1 (* 2 n (factorial (dec n))))) ; Unable to resolve symbol: factorial in this context

;; Aha! 

(defn factorial2 [n] (if (< n 2) 1 (* 2 n (factorial2 (dec n))))) ; #'user/factorial2

(factorial2 4) ; 192


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For convenience I'd like to make that a function:

(defn shred-user []
  (doseq [s (map first (ns-interns 'user))] (ns-unmap 'user s)))

(shred-user) ; nil

(shred-user) ; Unable to resolve symbol: shred-user in this context ; bugger!

(defn shred-user []
  (doseq [s (filter (complement #{'shred-user}) (map first (ns-interns 'user)))] (ns-unmap 'user s)))

(shred-user) ; nil

(shred-user) ; nil

