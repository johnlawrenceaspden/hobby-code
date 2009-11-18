from sudoku import printboard, parse_grid

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

printboard(parse_grid(hardestsudokuinworld))
#choose 3 from 36 (going across then down, find first pair) fails
#choose 6

printboard(parse_grid("""
850602400
720000009
004000000
000107002
305000900
040000000
000080070
017000000
000036040
"""))




