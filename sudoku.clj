;def cross(A, B):
;    return [a+b for a in A for b in B]

(defn cross [A, B]
  (for [a A b B] (str a b)))

;rows= 'ABCDEFGHI'
;cols= '123456789'
;digits = '123456789'

(def rows "ABCDEFGHI")
(def cols "123456789")
(def digits "123456789")
(def subsquaresize 3)
;(def rows "ABCD")
;(def cols "1234")
;(def digits "rgby")
;(def subsquaresize 2)
(def separators "0.-")

;squares = cross(rows, cols)

(def squares (cross rows cols))

;unitlist = ([cross(rows, c) for c in cols] +
;            [cross(r, cols) for r in rows] +
;            [cross(rs, cs) for rs in ('ABC', 'DEF', 'GHI') for cs in ('123', '456', '789')])

(def unitlist (map set  (concat 
                         (for [c cols] (cross rows [c]))
                         (for [r rows] (cross [r] cols))
                         (for [rs (partition subsquaresize rows) cs (partition subsquaresize cols)] (cross rs cs)))))

;units = dict((s, [u for u in unitlist if s in u]) for s in squares)
(defn dict [x] (apply sorted-map (apply concat x)))
(defn sett [x] (apply sorted-set (apply concat x)))

(def units (dict (for [s squares] 
                   [s (for [u unitlist :when (u s)] u)] )))

;peers = dict((s, set(s2 for u in units[s] for s2 in u if s2 !=s)) for s in squares)

(def peers (dict (for [s squares] 
                   [s (disj (sett (units s)) s)])))


;; def parse_grid(grid):
;;     "Given a string of 81 digits (or . or 0 or -), return a dict of {cell:values}"
;;     grid = [c for c in grid if c in '0.-123456789']
;;     values = dict((s,digits) for s in squares)
;;     for square,digit in zip(squares, grid):
;;         if digit in digits and not assign(values, square, digit):
;;             return False
;;     return values

(defn all? [coll] (every? identity coll))
(declare assign! eliminate! check!)

(defn parse_grid [grid]
  (let [grid (filter (set (concat digits separators)) grid)
        values (dict (for [s squares] [s,(atom digits)]))]
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
  (for [u (units s)] ;;for each row, column, and block associated with square s
    (let [dplaces (for [s u :when ((set @(values s)) d)] s)] ;;how many possible placings of d 
      (if (= (count dplaces) 0) ;;if none then we've failed
        false
        (if (= (count dplaces) 1) ;;if only one, then that has to be the answer
          (if (not (assign! values (first dplaces) d)) ;;so we can assign it.
            false
            values);;There's no point in this if. just call assign.
          values)))))


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

(defn print_board [values] (print (board values)))

(board (parse_grid ""))

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


;(printboard (parse_grid "..r.\n.g..\n....\n...."))
;((parse_grid "2345rgct6b") "A2")

;; '(




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
    


;; gridh01='4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......'

;; #[printboard(parse_grid(x)) for x in [gride01,gride02,gride03]]

;; easysudokufile=open("sudoku.txt").read().strip()
;; import re
;; easysudokus=[x for x in re.compile(r'\s*Grid\s.*\s*').split(easysudokufile) if x!='']


;; hardsudokus=open("sudoku_hard.txt").read().strip().split()

;; hardestsudokuinworld="""
;; 850002400
;; 720000009
;; 004000000
;; 000107002
;; 305000900
;; 040000000
;; 000080070
;; 017000000
;; 000036040
;; """

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