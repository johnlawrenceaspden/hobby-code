;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trampolining Your Way Through Sequence Space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's the levenshtein distance function that I stole, and which I was so
;; pleased with, until I realised that it broke for sequences longer than 140.

(defn levenshtein-distance-1
  "Calculates the edit-distance between two sequences"
  [seq1 seq2]
  (cond
   (empty? seq1) (count seq2)
   (empty? seq2) (count seq1)
   :else (min
          (+ (if (= (first seq1) (first seq2)) 0 1)
             (levenshtein-distance-1 (rest seq1) (rest seq2))) 
          (inc (levenshtein-distance-1 (rest seq1) seq2))      
          (inc (levenshtein-distance-1 seq1 (rest seq2))))))

;; As written, it's an exponential tree recursion, impractical for even short
;; strings. But we can memoize to make it feasible.

(def levenshtein-distance-1 (memoize levenshtein-distance-1))

;; This is equivalent to transforming the algorithm into filling out a grid.

(levenshtein-distance-1 "avada kedavra" "abracadabra") ;; 7

;; It's not desperately fast, however:
(time (levenshtein-distance-1 (repeat 100 \a) (repeat 100 \b))) ;; "Elapsed time: 233.114466 msecs"

;; And unfortunately, it requires real recursion, and the JVM won't play for
;; even rather short stack depths:
(levenshtein-distance-1 (repeat 140 \a) (repeat 140 \b)) ;; stack overflow

;; So I wondered if it were possible to implement proper recursion without using
;; the JVM stack

;; Here's all the machinery from the previous posts. I won't explain it again.

(def  fact-list (atom {}))
(defn add-fact! [n fn] (swap! fact-list #(assoc % n fn)))

(def  to-do-list (atom '()))
(defn add-task! [t] (swap! to-do-list #(cons t %)))
(defn add-tasks! [tlist] (doseq [t (reverse tlist)] (add-task! t)))
(defn pop-task! [] (let [h (first @to-do-list)] (swap! to-do-list rest) h))
(defn run-list! []
  (let [a (pop-task!)]
    (when (not (nil? a))
      (a)
      (recur))))

(defn peek-lists [] [fact-list to-do-list])
(defn init! [] (reset! fact-list {}) (reset! to-do-list '()))

(defn calculate-levenshtein-distance[m n]
  (init!)
  (let [a (levenshtein-distance m n)]
    (if (= a :tasks-added-to-do-list)
      (do 
        (run-list!)
        (levenshtein-distance m n))
      a)))

;; And here is the levenshtein-distance function itself:

(defn levenshtein-distance
  "Calculates the edit-distance between two sequences"
  [seq1 seq2]
  (let [return (fn [x] (add-fact! [seq1 seq2] x) x)]
    (cond
     (empty? seq1) (return (count seq2))
     (empty? seq2) (return (count seq1))
     :else (let [l1 (@fact-list [(rest seq1) (rest seq2)])
                 l2 (@fact-list [(rest seq1) seq2])
                 l3 (@fact-list [seq1 (rest seq2)])]
             (if (and l1 l2 l3)
               (return (min
                        (+ (if (= (first seq1) (first seq2)) 0 1)
                           l1) 
                        (inc l2)      
                        (inc l3)))
               (do (add-task! #(levenshtein-distance seq1 seq2))
                   (when (nil? l1) (add-task! #(levenshtein-distance (rest seq1) (rest seq2))))
                   (when (nil? l2) (add-task! #(levenshtein-distance (rest seq1) seq2)))
                   (when (nil? l3) (add-task! #(levenshtein-distance seq1 (rest seq2))))
                   :tasks-added-to-do-list))))))

;; It appears to be correct
(calculate-levenshtein-distance "abracadabra" "avada kedavra") ; 7

;; It doesn't stack overflow
(calculate-levenshtein-distance (repeat 140 \a) (repeat 140 \b)) ; 140

;; And it's not actually that much slower than the original:
(time (calculate-levenshtein-distance (repeat 100 \a) (repeat 100 \b)))   ; "Elapsed time: 342.621394 msecs"


;; Unfortunately, although it can seemingly deal correctly with arbitrary
;; sequences, performance becomes hellishly poor for moderately long sequences.
(time (calculate-levenshtein-distance (repeat 100 \a) (repeat 100 \b)))   ; "Elapsed time: 342.621394 msecs"
(time (calculate-levenshtein-distance (repeat 200 \a) (repeat 200 \b)))   ; "Elapsed time: 1624.51429 msecs"
(time (calculate-levenshtein-distance (repeat 300 \a) (repeat 300 \b)))   ; "Elapsed time: 3761.366628 msecs"
(time (calculate-levenshtein-distance (repeat 400 \a) (repeat 400 \b)))   ; "Elapsed time: 6652.731286 msecs"
(time (calculate-levenshtein-distance (repeat 500 \a) (repeat 500 \b)))   ; "Elapsed time: 10858.500264 msecs"
(time (calculate-levenshtein-distance (repeat 600 \a) (repeat 600 \b)))   ; "Elapsed time: 17312.638457 msecs"
(time (calculate-levenshtein-distance (repeat 700 \a) (repeat 700 \b)))   ; "Elapsed time: 25577.001232 msecs"
(time (calculate-levenshtein-distance (repeat 800 \a) (repeat 800 \b)))   ; "Elapsed time: 38125.374296 msecs"
(time (calculate-levenshtein-distance (repeat 900 \a) (repeat 900 \b)))   ; "Elapsed time: 53218.250258 msecs"
(time (calculate-levenshtein-distance (repeat 1000 \a) (repeat 1000 \b))) ; "Elapsed time: 72846.918093 msecs"

;; It looks like performance is n^2 log n, which sounds about right...
(map /
     (map #(/ % 341.) '(1624 3761 6652 10858 17312 25577 38125 53218 72846))
     (map #(* % % (Math/log %)) '(2 3 4 5 6 7 8 9 10)))
;; (1.7176955618795284 1.1154805250386235 0.8794728200140566 0.7913729876185648
;; 0.7870650999393282 0.7866406070256217 0.8400957422087569 0.8768891633653204
;; 0.9277599949772517)

;; But even so, the time constant here is very large.

;; I can imagine filling in a 1000x1000 grid, as is needed here, by hand if
;; necessary (and given a week!)  A computer should really be able to do this
;; sort of thing in milliseconds.

;; And I wonder where all that time is going?

;; I'm quite pleased with all this as a proof of concept, but I do wonder if I'd
;; ever want to use something like this in a real program!









