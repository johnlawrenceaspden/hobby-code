# song="""
# 000(67)/
# (11)(11)(21)/76(65)/(44)50(67)/(71)(11)(21)/
# 76(65)/45.0(66)/25/6-0(65)/
# (11)(11)(11)(76)/55(53)/4(45)(66)(66)/
# (77)7'3(33)/(33)(21),(44)(33)/456(67)/
# 11(11)(11)/(17)(65)4(65)/11(11)(17)/
# (65)(45)1(65)/66-0/(22)(#1#1)(77)(11)/
# (22)3#4.0/(#2#1)(76)(#5#5)(66)/(77)#12.#1/
# 7--.0/----/(66)(65)4/56(65)/
# (44)5.0(66)/(71)(23)(43)/21(65)/45.0-/
# 66/2-5-/6--0/0(03)(21)b7-/6---/----/
# """

# import re
# song=re.sub("\n", "", song)


song="""
0 0 0 s1 1 1 0 b1 ( 1 1 )
"""

from pypeg2 import *

class Note(Keyword):
    grammar = Enum( K("0"),
                    K("1"), K("b1" ), K("s1" ),
                    K("2"), K("b2" ), K("s2" ),
    )

print(parse("0 1", some(Note)))

subgroup="(", maybe_some(Note), ")"

print(parse("(0 1)", subgroup))
print(parse("(0 1 b2)", subgroup))

class SubGroup(List):
    grammar=subgroup

print(parse("(0 1)", SubGroup))


class Beat(List):
    grammar = [SubGroup, Note]

print(parse("( 0 1 )", Beat))
print(parse("1", Beat))


piece=maybe_some(beat)

print(parse("1 1 ( 0 0 1) (1 1) b1 0 0 s2", piece))



class Piece(List):
    grammar = maybe_some(Beat)

print(parse("1 1 ( 0 0 1) (1 1) b1 0 0 s2", Piece))

    
