#!/usr/bin/env python

print "hello"

# In Emacs can use C-c C-c to evaluate the buffer
# C-c C-z to switch to the interpreter window

import sys

# eval region is C-c C-r

# reading Peter Norvig's Sudoku Solver is always good as a python refresher

def cross(A,B):
    return [a+b for a in A for b in B]

rows='ABCDEFGHI'
cols='123456789'
digits='123456789'
squares=cross(rows, cols)

unitlist = ([cross(rows,c) for c in cols] + 
            [cross(r, cols) for r in rows] +
            [cross(rs, cs) for rs in ('ABC', 'DEF', 'GHI') for cs in ('123', '456', '789')])

units = dict((s, [u for u in unitlist if s in u]) for s in squares)

# units is a dictionary from squares to the units that they are in
## units['A1']

peers = dict((s, set(s2 for u in units[s] for s2 in u if s2 !=s)) for s in squares)

# peers is all the squares which are affected by a value being set for a square, so everything in its row, its column, and its 3x3 square
# except itself!
## peers['A1']
## set(['F1', 'B2', 'G1', 'B3', 'I1', 'H1', 'C3', 'A8', 'A3', 'A2', 'A5', 'B1', 'A7', 'A6', 'A9', 'A4', 'C1', 'E1', 'C2', 'D1'])

# It's a general convention that these functions return False if the grid can't be filled.

def parse_grid(grid):
    grid = [c for c in grid if c in '0.-123456789']
    values = dict((s,digits) for s in squares)
    for square, digit in zip(squares, grid):
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
    "Eliminate d from values[s]; propagate when values or places <= 2."
    if d not in values[s]:
        return values ## Already eliminated
    values[s] = values[s].replace(d,'')
    if len(values[s]) == 0: 
        return False ## Contradiction: removed last values
    elif len(values[s])==1:
        # If there's only one value (d2) left in the square, remove it from peers
        d2, =values[s]
        if not all(eliminate(values, s2, d2) for s2 in peers[s]):
            return False
    ## Now check the places where d appears in the units of s
    for u in units[s]:
        dplaces = [s for s in u if d in values[s]]
        if len(dplaces) == 0:
            return False
        elif len(dplaces) ==1:
            # d can only be in one place in a unit; assign it there
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


# a quick sanity check so far
# example = (parse_grid('4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......'))
# printboard(example)
# assign(example, 'H2','6')

def search(values, recurse=''):
    "Using depth-first search and propagation, try all possible values."
    print "recursion: ",recurse
    if values is False:
        return False
    if all(len(values[s])==1 for s in squares):
        return values
    _,s = min((len (values[s]),s) for s in squares if len(values[s])>1)
    for d in values[s]:
        result=search(assign(values.copy(), s, d), recurse+d)
        if result:
            return result
    return False

# example=parse_grid('4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......')
# search(example)
    
easysudokufile=open("sudoku.txt").read().strip()
import re
easysudokus=[x for x in re.compile(r'\s*Grid\s.*\s*').split(easysudokufile) if x!='']
