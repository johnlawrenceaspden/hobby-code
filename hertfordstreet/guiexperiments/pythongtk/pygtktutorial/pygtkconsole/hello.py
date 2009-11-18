w=Window()
b=Button('Hello')
w.add(b)
def hello(b):
    print "Hello, World!"

b.connect('clicked', hello)
w.show_all()
