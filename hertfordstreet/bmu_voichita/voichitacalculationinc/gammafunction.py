'''Calculation of the gamma function using code taken from the Wikipedia
entry on the Lanczos approximation'''
from math import sqrt, exp, pi, sin


# Coefficients used by the GNU Scientific Library
g = 7
p = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
     771.32342877765313, -176.61502916214059, 12.507343278686905,
     -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7]

def gamma(z):
    '''Lanczos approximation to the gamma function
    >>> gamma(-0.5)
    -3.5449077018110295
    >>> gamma(1.0)
    0.99999999999999978
    >>> gamma(10)
    362880.00000000151
    '''
    # Reflection formula
    if z < 0.5:
        return pi / (sin(pi*z)*gamma(1-z))
    else:
        z -= 1
        x = p[0]
        for i in range(1, g+2):
            x += p[i]/(z+i)
        t = z + g + 0.5
        return sqrt(2*pi) * t**(z+0.5) * exp(-t) * x

def test():
    import doctest, gammafunction
    return doctest.testmod(gammafunction)

if __name__=='__main__':
    print "testing", test()

    from pylab import plot,show,axis

    x=range(1200)
    xr=[10.0*(a/1000.0)-5.0005 for a in x]
    y=[gamma(x) for x in xr]
    plot(xr,y,'b.')
    axis([-5,10,-100,100])
    show()
        
