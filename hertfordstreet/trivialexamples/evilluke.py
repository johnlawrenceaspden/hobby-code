a = 3

def outer():
  a = 4
  def inner():
    global a
    print(a)
    a = 5
    print(a)
  return inner

print(outer()())
