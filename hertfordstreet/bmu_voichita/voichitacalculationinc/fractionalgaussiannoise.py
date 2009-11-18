from math import sin, pi, sqrt, exp
from gammafunction import gamma

def SpectralDensityFunction(f,H):
    '''Spectral density function (-0.5<f<0.5) for fractional Gaussian noise with Hurst Exponent H
    I don't know how to derive this formula. It is in Voichita's matlab code, and she refers to it as 
    "the almost exact formula from Percival and Walden".
    I have a feeling that it might be a Taylor series approximation to the Fourier transform of the auto-covariance relation
    for fractional Gaussian Noise.
    >>> SpectralDensityFunction(0.26,0.2367)
    1.1067908939278011
    ''' 
    if(H==0): 
        return 2*sin(pi*f)**2
    else:
        M=100  #sum from k = -M to M of 1/|k+f|^(2H+1)
        s=sum( [1/(abs(x+f)**(2*H+1)) for x in range(-M,M+1)] )
        
        s+=1/(2*H*(M+1-f)**(2*H))
        s+=1/(2*H*(M+1+f)**(2*H))
        s-=(2*H+1)*(2*H+2)*(2*H+3)/(720*(M+1-f)**(2*H+4))
        s-=(2*H+1)*(2*H+2)*(2*H+3)/(720*(M+1+f)**(2*H+4))

        s+=(2*H+1)/(12*(M+1-f)**(2*H+2))
        s+=(2*H+1)/(12*(M+1+f)**(2*H+2))
        s+=1/(2*(M+1-f)**(2*H+1))
        s+=1/(2*(M+1+f)**(2*H+1))
        
        s*=4*gamma(2*H+1)*sin(pi*H)*(sin(pi*f)**2)
        s/=(2*pi)**(2*H+1)

        return s

def IntegratedSpectralDensity(H, wsteps, quadraturesteps, i):
    if(H==0.0):
        if i==0:
            return 1-2**wsteps/pi*sin(pi/2**wsteps)
        else:
            x=2**(wsteps+1-i)
            return 1-(x/pi)*sin(2*pi/x)+(x/pi)*sin(pi/x)
    elif(H==1.0):
        if i==0:
            return 2**wsteps
        else:
            return 1e-50
    else:
        if i==0:
            return gamma(2*H+1)*sin(pi*H)/(2*pi)**(2*H-1)/(1-H)*2**((wsteps+1)*(2*H-1)-1)
        else:
            scale=2**(wsteps+2-i)

            xold=1.0
            yold=SpectralDensityFunction(xold/scale,H)
            sum=0
            for j in range(1,quadraturesteps+1):
                xnew=(1+float(j)/quadraturesteps)
                ynew=SpectralDensityFunction(xnew/scale,H)
                sum+=(xnew-xold)*(ynew+yold)
                xold=xnew
                yold=ynew
            return sum/2.0
        
def IntegratedSpectralDensityFunction(H,wsteps,quadraturesteps=20):
    '''
    Appears to be the expected values of the wavelet coefficients of a fractional Gaussian noise.
    In the general 0<H<1 fine scale case these are calculated by doing a numerical integration
    over a region of the Spectral Density. I have no idea where this formula comes from, and
    have copied it from Voichita's matlab code (the function SDF_Int), and then clarified the calculation.
    >>> IntegratedSpectralDensityFunction(0.0,5)
    [0.0016056069643816118, 0.011220690747206885, 0.044596134335340376, 0.17386272609022058, 0.62707677142194329, 1.6366197723675813]
    >>> IntegratedSpectralDensityFunction(1.0,5)
    [32, 1e-50, 1e-50, 1e-50, 1e-50, 1e-50]
    >>> IntegratedSpectralDensityFunction(0.76,5)
    [6.4284808246144252, 2.5337868118662028, 1.7592398286153024, 1.2077211418590617, 0.8012983421742671, 0.51762057939023354]
    >>> IntegratedSpectralDensityFunction(0.76,5,10)
    [6.4284808246144252, 2.5344386424271996, 1.7596935438138646, 1.2080370431239666, 0.80152942631348534, 0.5178955503380388]
    >>> IntegratedSpectralDensityFunction(0.35,8)
    [0.16634193518773813, 0.24327887427188533, 0.29962683660583778, 0.36932052594955755, 0.45630849916626082, 0.56762788846911971, 0.71869843738552097, 0.94205671645702882, 1.2302926459188461]
    '''

    return [IntegratedSpectralDensity(H,wsteps,quadraturesteps,i) for i in range(wsteps+1)]
    
def _test():
    import doctest, fractionalgaussiannoise
    return doctest.testmod(fractionalgaussiannoise)
     

if __name__=='__main__':
    print "testing", _test()
    from pylab import plot, axis, show, legend, figure, xlabel, ylabel, title
    x=range(-500,500)
    xr=[t/1000.0+0.0005 for t in x]

    HurstExponents=[x/10.0 for x in range(10)]

    import bmu

    figure()
    for H in HurstExponents:
        label='H=%0.1f' %H
        plot(xr, [SpectralDensityFunction(x,H)-bmu.SpectralDensityFunction(x,H) for x in xr], label=label)
    title('Difference between Python and C Function')
    xlabel('frequency')
    ylabel('spectral density')
    legend()
        
    
    figure()
    for H in HurstExponents:
        label='H=%0.1f'%H
        plot(xr, [SpectralDensityFunction(x,H) for x in xr], label=label)
    axis([-0.5,0.5,0,5])
    title('Spectral Density of fractional Gaussian Noise with Hurst Exponent H')
    xlabel('frequency')
    ylabel('spectral density')
    legend()

       
    figure()
    waveletsteps=10
    for H in HurstExponents:
        label='H=%0.1f'%H
        plot(range(waveletsteps+1),IntegratedSpectralDensityFunction(H,waveletsteps), label=label)
    title('Expected Results of Wavelet Transform on fractional Gaussian Noise')
    xlabel('wavelet scale')
    ylabel('expected value of coefficient')
    legend()

    figure()
    waveletsteps=10
    for H in HurstExponents:
        label='H=%0.1f'%H
        pyresults=IntegratedSpectralDensityFunction(H,waveletsteps)
        cresults=[bmu.IntegratedSpectralDensity(H,waveletsteps,20,i) for i in range(waveletsteps+1)]
        differences=[a-b for a,b in zip(pyresults,cresults)]
        plot(range(waveletsteps+1),differences, label=label)
    title('Differences between python and C functions')
    xlabel('wavelet scale')
    ylabel('expected value of coefficient')
    legend()
    show()
    
