;;Sudoku solver

;;Inspired by Peter Norvig's famous solver in Python
;;Which is explained in detail at:
;;http://norvig.com/sudoku.html

;;Algorithm is constraint propagation coupled with depth-first search

;;Constraint propagation in the original was performed by mutually recursive functions modifying state

;;I'm trying to get the functions to return a list of things to do rather than recurse, effectively nationalizing the machine stack.

;;I split the eliminate function into two (eliminate! and check!) to make it easier to read.

(defn cross [A, B]
  (for [a A b B] (str a b)))

(def rows "ABCDEFGHI")
(def cols "123456789")
(def digits "123456789")
;;the grid is divided into subsquares
(def subsquaresize 3)
(def rowgroups (partition subsquaresize rows))
(def colgroups (partition subsquaresize cols))

;;When we encode the grids as strings we may use any of these characters to encode blank squares
(def separators "0.-")

;;Squares are indexed by strings A1 -> I9
(def squares (cross rows cols))

;;units are the groups into which squares are grouped: rows, columns and subsquares
(def unitlist (map set  (concat 
                         (for [c cols] (cross rows [c]))
                         (for [r rows] (cross [r] cols))
                         (for [rs rowgroups
                               cs colgroups] (cross rs cs)))))

;;helper functions for making maps and sets
(defn dict [x] (apply sorted-map (apply concat x)))
(defn set-union [x] (apply sorted-set (apply concat x)))
;;use clojure's every? like python's all
(defn all? [coll] (every? identity coll))


;;which units are associated with a given square?
(def units (dict (for [s squares]  
                   [s (for [u unitlist :when (u s)] u)] )))

;;which other squares are linked to a given square through its units?
(def peers (dict (for [s squares]  
                   [s (disj (set-union (units s)) s)])))


;;three mutually recursive functions to propagate constraints. All of them return false 
;;if the constraints can not be satisfied.
(declare assign! eliminate! check!)

;;filter only the significant characters from an input string
(defn strip-grid [grid] (filter (set (concat digits separators)) grid))
;;make a grid where every square can contain every digit
(defn make-grid [] (dict (for [s squares] [s,(atom digits)])))

;;turn a string representing a grid into a dictionary of possible values for each square
(defn parse_grid [grid]
  (let [grid (strip-grid grid)
        values (make-grid)]
    (if (all? (for [[square digit] (zipmap squares grid) :when ((set digits) digit)]
                  (assign! values square digit)))
      values
      false)))

;;assign a definite value to a square by eliminating all other values.    
(defn assign! [values square digit]
  (if (all? (for [d @(values square) :when (not (= d digit))] 
              (eliminate! values square d)))
    values
    false))

;;remove a potential choice from a square. If that leaves no values, then that's a fail
;;if it leaves only one value then we can also eliminate that value from its peers.
;;either way, perform checks to see whether we've left the eliminated value with only one place to go.           
(defn eliminate! [values s d]
  (if (not ((set @(values s)) d)) values ;;if it's already not there nothing to do
      (do
        (swap! (values s) #(. % replace (str d) "")) ;;remove it
        (if (= 0 (count @(values s))) ;;no possibilities left
          false                       ;;fail
          (if (= 1 (count @(values s))) ;; one possibility left
            (let [d2 (first @(values s))]
              (if (not (all? (for [s2 (peers s)] (eliminate! values s2 d2))))
                false
                (check! values s d)))
            (check! values s d))))))

;;check whether the elimination of a value from a square has caused contradiction or further assignment
;;possibilities
(defn check! [values s d]
  (loop [u (units s)] ;;for each row, column, and block associated with square s
    (let [dplaces (for [s (first u) :when ((set @(values s)) d)] s)] ;;how many possible placings of d 
      (if (= (count dplaces) 0) ;;if none then we've failed
        false
        (if (= (count dplaces) 1) ;;if only one, then that has to be the answer
          (if (not (assign! values (first dplaces) d)) ;;so we can assign it.
            false
            (if (not (empty? (rest u))) (recur (rest u)) values))
          (if (not (empty? (rest u))) (recur (rest u)) values))))))

;;the function to print out the board is the hardest thing to translate from python to clojure!
(defn centre[s width]
  (let [pad (- width (count s))
        lpad (int (/ pad 2))
        rpad (- pad lpad)]
  (str (apply str (repeat lpad " ")) s (apply str (repeat  rpad " ")))))

(defn join [char seq]
  (apply str (interpose char seq)))

(defmacro forjoin [sep [var seq] body]
  `(join ~sep (for [~var ~seq] ~body)))

(defn board [values]
  (if (= values false)
    "no solution"
    (let [ width (+ 2 (apply max (for [s squares] (count @(values s)))))
          line (str \newline 
                    (join \+ (repeat subsquaresize 
                                     (join \- (repeat subsquaresize 
                                                      (apply str (repeat width "-"))))))
                    \newline)]
      (forjoin line [rg rowgroups]
               (forjoin "\n" [r rg]
                        (forjoin "|" [cg colgroups]
                                 (forjoin " " [c cg] 
                                          (centre @(values (str r c)) width))))))))

(defn print_board [values] (println (board values)))

;;We can't use Dr Norvig's trick of avoiding a deep copy by using strings. We have to copy the table
;;by recreating the atoms and copying their contents
(defn deepcopy [values] (dict (for [k (keys values)] [k (atom @(values k))])))

;;I've added a frill here where the search function keeps track of the search branches that it's following.
;;This means that we can print the branches out when debugging.
(defn search 
  ([values] (search values ""))
  ([values, recurse] 
     (println "recursion: " recurse)
     (if values
       (if (all? (for [s squares] (= 1 (count @(values s))))) ;;if all squares determined
         values                                               ;;triumph!
         (let [ pivot 
               (second (first (sort     ;;which square has fewest choices?
                               (for [s squares :when (>(count @(values s)) 1)] 
                                 [(count @(values s)),s]))))] 
           (let [results (for [d @(values pivot)] ;;try all choices
                           (do ;(print_board values)
                               (search (assign! (deepcopy values) pivot d) (str recurse d))))] ;(format "%s->%s;" pivot d)
                (some identity results)))) ;;and if any of them come back solved, return solution
         
       false)))


;;here's a demo:
(def hardestsudokuinworld "
850002400
720000009
004000000
000107002
305000900
040000000
000080070
017000000
000036040
")

(defn solve [grid]
     (do
       (println "\nproblem:")
       (println (join \newline (map #(apply str %) (partition 9 (filter (set (concat digits separators)) grid)))))
       (println "\nsolution:")
       (print_board (search (parse_grid grid)))))

(solve hardestsudokuinworld)

;;Dr Norvig provides a couple of files of easy and difficult sudokus for demonstration purposes.
;;Here is some code to read them in and solve them

(use 'clojure.contrib.str-utils)
(use 'clojure.contrib.duck-streams)

(def easy-sudokus (re-split #"\s*Grid\s.*\s*" (slurp "sudoku.txt")))
(def hard-sudokus (read-lines "sudoku_hard.txt"))

(defn show-off []
  (solve hardestsudokuinworld)
  (doall (map solve easy-sudokus))
  (doall (map solve hard-sudokus)))

;; Lessons learned during translation process

;; Lazy evaluation and mutation really don't work together very well.

;; Solver appeared to work but seemed to take infinite time on 3rd sudoku
;; Actually it took several hundred thousand iterations, but got the right answer
;; run next to python program showed that python code was getting there in a couple of hundred
;; Realised that constraints were not being propagated properly
;; Added doalls to every for
;; Now program crashes because last values have been eliminated without returning false
;; Actually we need loops with early return, otherwise we keep eliminating things from already false branches
;; Now notice that the doalls are actually making things slower because any? would have short-circuited once anything was false. Get rid of them and get a 2x speedup.
;; now running at half the speed of python