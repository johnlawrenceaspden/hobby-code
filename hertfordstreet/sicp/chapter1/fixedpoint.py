def findfixedpoint(func, initial, tolerance):
	a1=initial
	a2=func(initial)
	print a1
	while (abs(a1-a2)>tolerance):
		a1, a2 = a2, func(a2)
		print a1
	print a2
	return a2

def huronsqrtfn(x, a):
	return (x+a/x)/2

def sqrt(a):
	return findfixedpoint((lambda(x) :huronsqrtfn(x,a)), 1.0, 0.0001)
	
print sqrt(100)
print sqrt(1000)