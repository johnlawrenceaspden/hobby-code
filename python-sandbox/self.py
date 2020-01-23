# I always find python's self confusing when I haven't seen it for a while

# Interesting things from:
# https://stackoverflow.com/questions/2709821/what-is-the-purpose-of-the-word-self-in-python#2709832


# The equivalence of calling a method on an object and calling the method via the class with the object as an argument
# https://stackoverflow.com/a/21366809
class ClassA():
    def methodA(self, arg1, arg2):
        print(self, arg1, arg2)

a=ClassA()

ClassA.methodA(a,1,2)
a.methodA(1,2)


# Objects are a namespacing mechanism for types of dictionaries
# https://stackoverflow.com/a/6433556

# Without objects
def state_init(state):
    state['field'] = 'init'

def state_add(state, x):
    state['field'] += x

def state_mult(state, x):
    state['field'] *= x

def state_getField(state):
    return state['field']

myself = {}
state_init(myself)
state_add(myself, 'added')
state_mult(myself, 2)

print( state_getField(myself) )
#--> 'initaddedinitadded'

#With objects

class State(object):
    def __init__(self):
        self.field = 'init'
    def add(self, x):
        self.field += x
    def mult(self, x):
        self.field *= x

s = State()
s.add('added')    # self is implicitly passed in
s.mult(2)         # self is implicitly passed in
print( s.field )


#With objects, but with explicit selfing
s=State.__new__(State) #can create an object without calling __init__, like this.
State.__init__(s) 
State.add(s,'added')
State.mult(s,2)
print(s.field)


# Demonstrating the difference between  a class variable and an instance variable
# https://stackoverflow.com/a/17260649

class MyClass(object):
    i = 123
    def __init__(self):
        self.i = 345

a = MyClass()
print(a.i)
print(MyClass.i)



# Some ways of looking at the attributes
from pprint import pprint
pprint(a.__dict__)
pprint(MyClass.__dict__)
pprint(vars(a))
pprint(vars(MyClass))
