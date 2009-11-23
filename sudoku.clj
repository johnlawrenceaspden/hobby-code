;;Sudoku solver

;;As direct a translation as I could make of Peter Norvig's famous python solver
;;Which is explained in detail at:
;;http://norvig.com/sudoku.html

;;Algorithm is constraint propagation coupled with depth-first search

;;Constraint propagation is performed by mutually recursive functions modifying state
;;So in clojure we need to put our strings in atoms.

(defn cross [A, B]
  (for [a A b B] (str a b)))

(def rows "ABCDEFGHI")
(def cols "123456789")
(def digits "123456789")
(def subsquaresize 3)

(def separators "0.-")

;;Squares indexed by strings A1 -> I9
(def squares (cross rows cols))

;;units are the groups into which squares are grouped: rows, columns and subsquares
(def unitlist (map set  (concat 
                         (for [c cols] (cross rows [c]))
                         (for [r rows] (cross [r] cols))
                         (for [rs (partition subsquaresize rows) 
                               cs (partition subsquaresize cols)] (cross rs cs)))))

;;helper functions for making maps and sets
(defn dict [x] (apply sorted-map (apply concat x)))
(defn set-union [x] (apply sorted-set (apply concat x)))

;;which units are associated with a given square?
(def units (dict (for [s squares]  
                   [s (for [u unitlist :when (u s)] u)] )))

(def peers (dict (for [s squares]  
                   [s (disj (set-union (units s)) s)])))


(defn all? [coll] (every? identity coll))
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

    
;; def assign(values, square, digit):
;;     "Eliminate all the other values (except d) from values[s] and propagate."
;;     if all(eliminate(values, square, d2) for d2 in values[square] if d2!=digit):
;;         return values
;;     else:
;;         return False


(defn assign! [values square digit]
  ;(println "assign! " square digit)
  (if (all? (for [d @(values square) :when (not (= d digit))] 
              (eliminate! values square d)))
    values
    false))
           
;; def eliminate(values, s, d):
;;     "Eliminate d from values[s]; propagate when values or places <=2."
;;     if d not in values[s]:
;;         return values ##Already eliminated
;;     values[s] = values[s].replace(d,'')
;;     if len(values[s]) == 0:
;;         return False ##Contradiction: removed last value
;;     elif len(values[s])==1:
;;         ##If there is only one value (d2) left in square, remove it from peers
;;         d2, =values[s]
;;         if not all(eliminate(values, s2, d2) for s2 in peers[s]):
;;             return False
;;     ## Now check the places where d appears in the units of s
;;     for u in units[s]:
;;         dplaces = [s for s in u if d in values[s]]
;;         if len(dplaces) == 0:
;;             return False
;;         elif len(dplaces) ==1:
;;             # d can only be in one place in unit; assign it there
;;             if not assign(values, dplaces[0], d):
;;                 return False
;;     return values

(defn eliminate! [values s d]
 ; (println "eliminate! " s d)
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

(defn check! [values s d]
;  (println "check! " s d)
  (loop [u (units s)] ;;for each row, column, and block associated with square s
 ;   (println u)
    (let [dplaces (for [s (first u) :when ((set @(values s)) d)] s)] ;;how many possible placings of d 
  ;    (println dplaces)
      (if (= (count dplaces) 0) ;;if none then we've failed
        false
        (if (= (count dplaces) 1) ;;if only one, then that has to be the answer
          (if (not (assign! values (first dplaces) d)) ;;so we can assign it.
            false
            (if (not (empty? (rest u))) (recur (rest u)) values))
          (if (not (empty? (rest u))) (recur (rest u)) values))))))


(defn centre[s width]
  (let [pad (- width (count s))
        lpad (int (/ pad 2))
        rpad (- pad lpad)]
  (str (apply str (repeat lpad " ")) s (apply str (repeat  rpad " ")))))


;; def printboard(values):
;;     "Used for debugging."
;;     if values==False:
;;         print 'no solution'
;;     else:
;;         width = 1 + max(len(values[s]) for s in squares)
;;         line = '\n' + '+'.join(['-'*(width*3)]*3)
;;         for r in rows:
;;             print ''.join(values[r+c].center(width)+(c in '36' and '|' or '')
;;                           for c in cols) + (r in 'CF' and line or '')
;;         print

(defn join [char seq]
  (apply str (interpose char seq)))

(defmacro forjoin [sep [var seq] body]
  `(join ~sep (for [~var ~seq] ~body)))


(defn board [values]
  (if (= values false)
    "no solution"
  (let [rgr  (partition subsquaresize rows)
        cgr  (partition subsquaresize cols)
        width (+ 2 (apply max (for [s squares] (count @(values s)))))
        line (str \newline 
                  (join \+ (repeat subsquaresize 
                    (join \- (repeat subsquaresize 
                       (apply str (repeat width "-"))))))
                  \newline)]
    (forjoin line [rg rgr]
             (forjoin "\n" [r rg]
                      (forjoin "|" [cg cgr]
                               (forjoin " " [c cg] 
                                        (centre @(values (str r c)) width))))))))

(defn print_board [values] (println (board values)))

;(print_board (parse_grid "rgby|ybrg|g...|...."))
;(print_board (parse_grid "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"))

(print_board (parse_grid  "
850002400
720000009
004000000
000107002
305000900
040000000
000080070
017000000
000036040
"))

;(def tv (parse_grid "rgby|y.rg|g...|...."))
;(def tv (parse_grid "r.b.|y...|g...|...."))

(defn deepcopy [values] (dict (for [k (keys values)] [k (atom @(values k))])))

;; def search(values, recurse=''):
;;     "Using depth-first search and propagation, try all possible values."
;;     print "recursion: ", recurse
;;     if values is False:
;;         return False
;;     if all(len(values[s])==1 for s in squares):
;;         return values
;;     _,s = min((len(values[s]),s) for s in squares if len(values[s])>1)
;;     for d in values[s]:
;;         result=search(assign(values.copy(), s, d), recurse+d)
;;         if result:
;;             return result
;;     return False
    
(defn search 
  ([values] (search values ""))
  ([values, recurse] 
     ;(println "recursion: " recurse)
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


;; gridh01='4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......'

;; #[printboard(parse_grid(x)) for x in [gride01,gride02,gride03]]

;; easysudokufile=open("sudoku.txt").read().strip()
;; import re
;; easysudokus=[x for x in re.compile(r'\s*Grid\s.*\s*').split(easysudokufile) if x!='']

(use 'clojure.contrib.str-utils)
(def easy-sudokus (re-split #"\s*Grid\s.*\s*" (slurp "sudoku.txt")))


;; hardsudokus=open("sudoku_hard.txt").read().strip().split()

(use 'clojure.contrib.duck-streams)
(defn show-off []
  (for [l (read-lines "sudoku_hard.txt")]
    (print_board (search (parse_grid l)))))

(def hard-sudokus (read-lines "sudoku_hard.txt"))

;(print_board (search (parse_grid (first hard-sudokus))))
;(print (join \newline (map #(apply str %) (partition 9 (first hard-sudokus)))))

(defn solve [grid]
     (do
       (println (join \newline (map #(apply str %) (partition 9 (filter (set (concat digits separators)) grid)))))
       (print_board (search (parse_grid grid)))))

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

(defn show-off []
  (doall (map solve hard-sudokus))
  (doall (map solve easy-sudokus))
  (solve hardestsudokuinworld))




;; def solve(sudoku):
;;     print sudoku
;;     values=parse_grid(sudoku)
;;     printboard(values)
;;     printboard(search(values))

;; def showoff(slist):
;;     for x in slist:
;;         solve(x)

;; def test():
;;     showoff(easysudokus)
;;     showoff(hardsudokus)
;;     showoff([hardestsudokuinworld])

;; )


;; Lessons learned
;;lazy evaluation and mutation really don't work together very well.

;; Solver appeared to work but seemed to take infinite time on 3rd sudoku
;; Actually it took several hundred thousand iterations, but got the right answer
;; run next to python program showed that python code was getting there in a couple of hundred
;; Realised that constraints were not being propagated properly
;; Added doalls to every for
;; Now program crashes because last values have been eliminated without returning false
;; Actually we need loops with early return, otherwise we keep eliminating things from already false branches
;; Now notice that the doalls are actually making things slower because any? would have short-circuited once anything was false. Get rid of them and get a 2x speedup.
;; now running at half the speed of python