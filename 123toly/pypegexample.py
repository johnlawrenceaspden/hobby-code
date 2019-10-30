prog="""
int func(int a, long b)
{ 
    do_this; 
    do_that; 
}
int func(int a, long b)
{ 
    do_this; 
    do_that; 
}

"""

from pypeg2 import *

class Type(Keyword):
    grammar = Enum( K("int"), K("long") )

class Parameter:
    grammar = attr("typing", Type), name()

class Parameters(Namespace):
    grammar = optional(csl(Parameter))

class Instruction(str):
    grammar = word, ";"

block = "{", maybe_some(Instruction), "}"

class Function(List):
    grammar = attr("typing", Type), name(), \
            "(", attr("parms", Parameters), ")", block

program= maybe_some(Function)

f = parse(prog, program)

print(f)
