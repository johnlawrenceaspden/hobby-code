'''
Playing with the idea of the Hurst exponent:
    Generate time series of various lengths, and their 'rescaled range' statistic,
    Use gnuplot to plot log(RS) against log(n) for various methods of generating series
'''

import random, sys, os
import Gnuplot
import Numeric
import math

def RSstatistic(series):
    "for a given series, the range over the standard deviation"
    sum=0
    min=max=series[0]
    for x in series:
        sum+=x
        if(sum<min): min=sum
        if(sum>max): max=sum
    mean=sum/(len(series))
    srange=max-min

    deviations=[x-mean for x in series]
    sqsum=0
    for x in deviations:
        sqsum+=x*x
    stddev=math.sqrt(sqsum/len(series))

    return srange/stddev
    
def createscatterfile(xdata, ydata, filename):
    '''create a file "filename" with the given data in it so that gnuplot can plot it'''
    assert(len(xdata)==len(ydata))
    dfile=open(filename,'w')
    for i in range(len(xdata)):
        dfile.write('%s %s\n' % (xdata[i], ydata[i]))
    dfile.close()

def randomwalk(n):
    '''Generate a gaussian random walk of length n'''
    walk=[]
    w=0
    for i in range(n):
            w+=random.gauss(0,1)
            walk.append(w)
    return walk

def whitenoise(n):
    '''Generate white noise of length n'''
    noise=[]
    for i in range(n):
        noise.append(random.gauss(0,1))
    return noise

def createhurstplot(timeseriesgeneratorfn, filename):
    '''Given a time series generator, create a file "filename" suitable for gnuplot
    containing log n against log RS for many series of length n'''
    xdata=[]
    ydata=[]
    n=5
    while (n<1000):    
        x=math.log(n)
        y=math.log(RSstatistic(timeseriesgeneratorfn(n)))
        xdata.append(x); ydata.append(y)
        n*=1.03
    createscatterfile(xdata, ydata, filename)
   
createhurstplot(whitenoise, "whitenoise")
createhurstplot(randomwalk, "randomwalk")

cfile=open('gnuplot.commands','w')
cfile.write('plot "whitenoise", "randomwalk" \n')
cfile.close()

os.spawnl(os.P_WAIT, '/usr/bin/gnuplot', 'gnuplot', '-persist', 'gnuplot.commands')





