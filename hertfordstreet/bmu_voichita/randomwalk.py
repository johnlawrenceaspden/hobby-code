'''
Understanding the Hurst exponent.
Calculate a random walk and then calculate various statistics for it:
    The mean and standard deviation of the walk
    the deviations from the mean
    their sums
    the range of their sums
Use gnuplot to produce a graph of these things
'''

import random, sys, os
import Gnuplot
import Numeric
import math


def createfile(data, filename):
    dfile=open(filename,'w')
    for i in range(len(data)):
        dfile.write('%s %s\n' % (i, data[i]))
    dfile.close()
   

def randomwalk(n):
    walk=[]
    w=0
    for i in range(n):
            w+=random.gauss(0,1)
            walk.append(w)
    return walk

def mean(walk):
    sum=0
    for i in walk:
        sum+=i
    return sum/len(walk)

def deviations(walk):
    m=mean(walk)
    return [d-m for d in walk]
   
def sums(series):
    s=[]
    sum=0
    for x in series:
        sum+=x
        s.append(sum)
    return s

def srange(series):
    min=max=series[0]
    for x in series:
        if x>max: max=x
        if x<min: min=x

    return min, max

def standarddeviation(series):
    m=mean(series)
    devs=deviations(series)
    sum=0
    for x in devs:
        sum += x*x
    return math.sqrt(sum/len(series))
        


walk=randomwalk(100)
devs=deviations(walk)
devsums=sums(devs)
min, max = srange(devsums)
stddev=standarddeviation(walk)

createfile(walk, 'walk')
createfile(devs, 'devs')
createfile(devsums, 'sums')


cfile=open('gnuplot.commands','w')
cfile.write('plot "walk"\n')
cfile.write('replot %s\n' % mean(walk))
cfile.write('replot "devs"\n')
cfile.write('replot "sums"\n')
cfile.write('replot %s\n' % min)
cfile.write('replot %s\n' % max)
cfile.write('replot %s\n' % stddev)
cfile.write('replot %s\n' % -stddev)
cfile.close()

os.spawnl(os.P_WAIT, '/usr/bin/gnuplot', 'gnuplot', '-persist', 'gnuplot.commands')


