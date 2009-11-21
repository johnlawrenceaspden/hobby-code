;def cross(A, B):
;    return [a+b for a in A for b in B]

(defn cross [A, B]
  (for [a A b B] (str a b)))

(cross (list \a) "ab")

;rows= 'ABCDEFGHI'
;cols= '123456789'
;digits = '123456789'

(def rows "ABCDEFGHI")
(def cols "123456789")
(def digits "123456789")

;squares = cross(rows, cols)

(def squares (cross rows cols))

;unitlist = ([cross(rows, c) for c in cols] +
;            [cross(r, cols) for r in rows] +
;            [cross(rs, cs) for rs in ('ABC', 'DEF', 'GHI') for cs in ('123', '456', '789')])

(def unitlist (map set  (concat 
                         (for [c cols] (cross rows [c]))
                         (for [r rows] (cross [r] cols))
                         (for [rs (partition 3 rows) cs (partition 3 cols)] (cross rs cs)))))

;units = dict((s, [u for u in unitlist if s in u]) for s in squares)
(defn dict [x] (apply hash-map (apply concat x)))
(defn sett [x] (set (apply concat x)))

(def units (dict (for [s squares] 
                   [s (for [u unitlist :when (u s)] u)] )))

;(def units2 (dict (for [s squares] [s (filter #(% s) unitlist)])))



;peers = dict((s, set(s2 for u in units[s] for s2 in u if s2 !=s)) for s in squares)

(def peers (dict (for [s squares] 
                   [s (disj (sett (units s)) s)])))







def parse_grid(grid):
    "Given a string of 81 digits (or . or 0 or -), return a dict of {cell:values}"
    grid = [c for c in grid if c in '0.-123456789']
    values = dict((s,digits) for s in squares)
    for square,digit in zip(squares, grid):
        if digit in digits and not assign(values, square, digit):
            return False
    return values


def assign(values, square, digit):
    "Eliminate all the other values (except d) from values[s] and propagate."
    if all(eliminate(values, square, d2) for d2 in values[square] if d2!=digit):
        return values
    else:
        return False

def eliminate(values, s, d):
    "Eliminate d from values[s]; propagate when values or places <=2."
    if d not in values[s]:
        return values ##Already eliminated
    values[s] = values[s].replace(d,'')
    if len(values[s]) == 0:
        return False ##Contradiction: removed last value
    elif len(values[s])==1:
        ##If there is only one value (d2) left in square, remove it from peers
        d2, =values[s]
        if not all(eliminate(values, s2, d2) for s2 in peers[s]):
            return False
    ## Now check the places where d appears in the units of s
    for u in units[s]:
        dplaces = [s for s in u if d in values[s]]
        if len(dplaces) == 0:
            return False
        elif len(dplaces) ==1:
            # d can only be in one place in unit; assign it there
            if not assign(values, dplaces[0], d):
                return False
    return values

def printboard(values):
    "Used for debugging."
    if values==False:
        print 'no solution'
    else:
        width = 1 + max(len(values[s]) for s in squares)
        line = '\n' + '+'.join(['-'*(width*3)]*3)
        for r in rows:
            print ''.join(values[r+c].center(width)+(c in '36' and '|' or '')
                          for c in cols) + (r in 'CF' and line or '')
        print


def search(values, recurse=''):
    "Using depth-first search and propagation, try all possible values."
    print "recursion: ", recurse
    if values is False:
        return False
    if all(len(values[s])==1 for s in squares):
        return values
    _,s = min((len(values[s]),s) for s in squares if len(values[s])>1)
    for d in values[s]:
        result=search(assign(values.copy(), s, d), recurse+d)
        if result:
            return result
    return False
    


gridh01='4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......'

#[printboard(parse_grid(x)) for x in [gride01,gride02,gride03]]

easysudokufile=open("sudoku.txt").read().strip()
import re
easysudokus=[x for x in re.compile(r'\s*Grid\s.*\s*').split(easysudokufile) if x!='']


hardsudokus=open("sudoku_hard.txt").read().strip().split()

hardestsudokuinworld="""
850002400
720000009
004000000
000107002
305000900
040000000
000080070
017000000
000036040
"""

def solve(sudoku):
    print sudoku
    values=parse_grid(sudoku)
    printboard(values)
    printboard(search(values))

def showoff(slist):
    for x in slist:
        solve(x)

def test():
    showoff(easysudokus)
    showoff(hardsudokus)
    showoff([hardestsudokuinworld])






    
