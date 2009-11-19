def square(x):
	return x*x

def improve_guess(x, guess):
	return ((guess+x/guess)/2)

def good_enough(x, guess, tolerance):
	return ( abs(square(guess) - x) < tolerance)

def sqrt_helper(x, guess, tolerance):
	print guess
	if good_enough(x, guess, tolerance):
		return guess
	else:
		return sqrt_helper(x, improve_guess(x, guess), tolerance)
	
def sqrt(x):
	return sqrt_helper(x*1.0, x/2, 0.0001)
	
print sqrt(10000000)