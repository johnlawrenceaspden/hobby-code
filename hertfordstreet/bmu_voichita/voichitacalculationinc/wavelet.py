import math

Haar = [1/math.sqrt(2), 1/math.sqrt(2)]
Daubechies4 = [ .482962913145, .836516303738, .224143868042, -.129409522551  ]


def log2(n):
    '''
    >>> log2(128)
    7
    '''
    return int(round(math.log(n)/math.log(2)))

def dyadlength(x):
    '''
    >>> dyadlength([1,2,3,4,5,6,7,8])
    (8, 3)'''
    l=len(x)
    return l, log2(l)

def everyotherelementof(d):
    '''
    >>> everyotherelementof([1,2,3,4])
    [1, 3]
    '''
    y=[]
    index=0
    for x in d:
        if(index==0):
            index=1
            y.append(x)
        else:
            index=0
    return y

def MirrorFilter(qmf):
    '''
    >>> MirrorFilter([1,2,3,4])
    [1, -2, 3, -4]
    '''
    mirror=[]
    index=0
    for x in qmf:
        if(index==0):
            mirror.append(x)
            index=1
        else:
            mirror.append(-x)
            index=0
    return mirror

def lshift(x):
    '''
    >>> lshift([1,2,3,4])
    [2, 3, 4, 1]
    '''
    a = x[1:]
    a.append(x[0])
    return a

def convolve(filter, signal, sense):
    '''
    >>> convolve([100,10,1],[1,2,3,4,5], True)
    [154, 215, 321, 432, 543]
    >>> convolve([100,10,1],[1,2,3,4,5], False)
    [123, 234, 345, 451, 512]
    '''
    filtered=signal[:]
    for i in range(len(signal)):
        filtered[i]=0
        for j in range(len(filter)):
            if(sense):
                filtered[i]+=filter[j]*signal[(i-j) % len(signal)]
            else:
                filtered[i]+=filter[j]*signal[(i+j) % len(signal)]
               
    return filtered   

def iconv(filter, signal):
    '''
    >>> iconv([100,10,1],[1,2,3,4,5])
    [154, 215, 321, 432, 543]
    '''
    return convolve(filter, signal, True)

def aconv(filter, signal):
    '''
    >>> aconv([100,10,1],[1,2,3,4,5])
    [123, 234, 345, 451, 512]
    '''
    return convolve(filter, signal, False)
    

def DownSampleHigh(x, qmf):
    '''
    >>> DownSampleHigh([7,2,4,8,34,1.3,2], Daubechies4)
    [-4.2731681844757006, 1.8717924841490001, -25.502913505463503, 6.3990185797276]
    '''
    d=iconv(MirrorFilter(qmf), lshift(x))
    return everyotherelementof(d)


def DownSampleLow(x,qmf):
    '''
    >>> DownSampleLow([7,2,4,8,34,1.3,2], Daubechies4)
    [4.9150722912509996, 16.0766412165957, 17.0506313200164, 6.7521895983360007]
    '''
    d=aconv(qmf,x)
    return everyotherelementof(d)
    
    
def PeriodizedOrthogonalWaveletTransform(x, L, qmf):
    '''
    >>> PeriodizedOrthogonalWaveletTransform([3,2,3.7,24,25,67,79,8],2,Haar)
    [3.5355339059327373, 19.586857838867363, 65.053823869162372, 61.518289963229634, -0.70710678118654746, 14.354267658086913, 29.698484809834998, -50.204581464244868]
    '''
    n,J=dyadlength(x)
    beta=x[:]
    wavelets=[]

    for j in range(J-1,L-1,-1):
        alfa = DownSampleHigh(beta,qmf)
        alfa.extend(wavelets)
        wavelets=alfa
        beta = DownSampleLow(beta,qmf)

    beta.extend(wavelets)
    return beta


def PeriodizedDaubechies4WaveletTransform(x,L):
    '''
    >>> PeriodizedDaubechies4WaveletTransform([1,2,3,4,5,6,7,8],2)
    [2.3107890345429998, 5.1392161592910002, 7.9676432840390001, 10.038195644859002, 2.8284271247450001, 1.0001166561579566e-12, 9.999778782798785e-13, 9.9964481137249095e-13]
    '''
    return PeriodizedOrthogonalWaveletTransform(x, L, Daubechies4)
    
def cversionPD4WT(x,L):
    import bmu
    a=bmu.doubleArray(len(x))
    b=bmu.doubleArray(len(x))
    for i,s in enumerate(x):
        a[i]=s
    bmu.PeriodizedDaubechies4WaveletTransform(a,len(x),b,L)
    retval=[]
    for i in range(len (x)):
        retval.append(b[i]);
    return retval
        

def _test():
    import doctest, wavelet
    return doctest.testmod(wavelet, verbose=False)

if __name__=="__main__":
    print "testing",_test()
    
    from math import sin,pi
    J=8
    N=2**J
    x=range(0,N)
    xr=[1.0*t/N for t in x]
    s=[sin(50*2.0*pi*t)+10*t*(1-t) for t in xr]
    #s=[10*t*(1-t) for t in xr]
    from pylab import plot, show, title,axis
    plot(x,s)
    for i in range (J):
        a=[q+(J-i)*10 for q in PeriodizedOrthogonalWaveletTransform(s,i,Daubechies4)]
        plot(x,a)
        b=[1+q+(J-i)*10 for q in cversionPD4WT(s,i)]
        plot(x,b)
    title("Wavelet Transform of %d element time series\nShowing each successive stage\nC version side by side with python" % N)
    axis(xmin=0, xmax=N)
    show()



