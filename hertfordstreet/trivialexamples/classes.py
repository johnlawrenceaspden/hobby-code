class Loaf:
    "loaves of bread with names, tastes and weights"
    ingredients=['yeast', 'flour']
    counter=0
    def taste(self):
        return "yeasty"
    def weight(self):
        return "3lb 2oz"
    def __init__(self, name="default loaf"):
        print 'init'
        print self.__class__
        self.__class__.counter += 1
        self.name=name
    def getname(self):
        return self.name
    def __repr__(self):
        "display name, taste and weight of the loaf"
        return "\n".join([self.getname(), self.taste(), self.weight()])
        

class MaltLoaf(Loaf):
    "malt loaves"
    ingredients=Loaf.ingredients+['malt', 'fruit']
    def __init__(self, name="soreen"):
        Loaf.__init__(self, name)
    def taste(self):
        return "malty"

from UserDict import UserDict

def __p():
  print Loaf.counter is MaltLoaf.counter
  print Loaf.counter
  print MaltLoaf.counter
  print

if __name__=='__main__':
    a=Loaf()
    __p()
    b=MaltLoaf()
    __p()
    c=Loaf()
    __p()
    d=MaltLoaf()
    __p()


