# Example pylab histogram program

import random, pylab

x=[22, 54, 55, 56, 58, 58, 58, 60, 62, 66, 68, 68]

pylab.ioff()
pylab.hist(x)  #, bins=125, normed=True, alpha=0.5, range=[0.0, 10.0], label='N=%i' % N)
pylab.title('$Scores$', fontsize=18)

pylab.xlabel('grades')
pylab.ylabel('students')
#pylab.ylabel('can use tex $\pi(\Sigma_N / N)$', fontsize=18)

pylab.axvline(sum(x)*1.0/len(x), c='k', lw=2.0, ls='--')


pylab.legend()
#pylab.xlim(1.0, 10.0)
pylab.show()


#pylab.savefig('histo_direct_gamma_average.png')
python.ion()
