/* This file contains functions to perform a certain calculation designed by Voichita Maxim
 * for the Brain Mapping Unit of the University of Cambridge.
 *
 * The calculation was originally embodied in a set of MATLAB functions.
 *
 * John Lawrence Aspden / J.L. Aspden Ltd translated in August 2006, under contract to the BMU, 
 * the MATLAB code to this form in C, without fully understanding the details of the 
 * calculation, and tested that it gave the same answers as the original MATLAB code 
 * with a wide variety of inputs.
 *
 * There was also an intermediate version in python, which was rather more readable due to the 
 * greater expressiveness of that language, which eventually became a test harness for 
 * the C code. This test harness, although not a deliverable, is available 
 * from J.L. Aspden Ltd on request.
 *
 * This algorithm is not for the faint of heart! 
 * Prerequisites for understanding this code are a knowledge of the wavelet transform 
 * and the process known as fractional Gaussian Noise (fGn)
 *
 * The input to the primary function Voichita Number
 * is a signal (time series) whose length is a power of two, and a set of candidate 
 * Hurst exponents. (In this case hsteps=100 corresponds to {0.0,0.01,0.02...0.99,1.0})
 *
 * The algorithm was designed on the premise that this input is a sample of an fGn.
 *
 * The code performs the wavelet transform on the input data.
 *    Daubechies 4 wavelets are used.
 *    The 'extra data at the endpoints' problem is solved (bodged) 
 *    using the periodic assumption, ie assuming signal repeats after the sample.
 *    This clearly can produce artifacts, we assume that they are minor 
 *    (We assume that Voichita assumed them to be minor)
 *
 * It also calculates, for each candidate Hurst Exponent, numbers which appear to be 
 * the expected energy at each wavelet scale for a noise with this H (and sigma=1?). 
 * The calculation is a numerical integration of (an approximation to) the 
 * Spectral Density function of the noise. This produces a two dimensional table.
 *
 * It then performs an iteration procedure in which it appears to be repeatedly 
 * re-estimating first H and then sigma, in the hope of converging on the maximum 
 * likelihood estimator. (Which I take to be the question: Which (H,sigma) from our
 * candidate set of H, sigma real, is most likely to generate the set of 
 * 'wavelet spectral coefficients' which we're seeing from our signal?)
 *
 * As far as we can tell, apart from the original MATLAB code, this algorithm and
 * the assumptions and mathematics on which it is based are not documented. 
 * As a result the comments in this code are guesswork and should be treated with suspicion.
 *
 * The reader trying to understand the algorithm for its own sake is referred to the 
 * original MATLAB code, or to the transitional python program, in which the computational 
 * details are less prominent, and the underlying mathematical operations more obvious.
 *
 * John Lawrence Aspden Saturday 26th August 2006
 */

#include<math.h>
#include<stdio.h>
#include<assert.h>
#include<stdbool.h>
#include"bmu.h"

#define M_PI 3.14159265358979323846264338327
#define MAX_SIGNAL_LENGTH 4096
#define MAX_HSTEPS 1024
#define MAX_WAVELETSTEPS 16
#define QUADRATURESTEPS 20
#define TOLERANCE (1e-20)

/*Coefficients used in the wavelet transform for a Daubechies 4 wavelet*/
const double Daubechies4[]  = { .482962913145,  .836516303738, .224143868042, -.129409522551};
const int    Daubechies4len = 4;

/* Given a fractional Gaussian noise of Hurst Exponent H, what is the spectral density for frequency f?
 * Voichita referred to this as 'the almost exact formula from Percival and Walden'*/
static double SpectralDensityFunction(double f,double H){
    if(H==0){ 
        double a=sin(M_PI*f);
        return 2*a*a;
    } else {
        int M=100;  //sum from k = -M to M of 1/|k+f|^(2H+1)
        double s=0;
        for(int k=-100; k<=100; k+=1){
            s+=1/(pow(fabs(k+f),2*H+1));
        }
        
        s+=1/(2*H*pow((M+1-f),(2*H)));
        s+=1/(2*H*pow((M+1+f),(2*H)));
        s-=(2*H+1)*(2*H+2)*(2*H+3)/(720*pow((M+1-f),(2*H+4)));
        s-=(2*H+1)*(2*H+2)*(2*H+3)/(720*pow((M+1+f),(2*H+4)));
        s+=(2*H+1)/(12*pow((M+1-f),(2*H+2)));
        s+=(2*H+1)/(12*pow((M+1+f),(2*H+2)));
        s+=1/(2*pow((M+1-f),(2*H+1)));
        s+=1/(2*pow((M+1+f),(2*H+1)));
        
        s*=4*tgamma(2*H+1)*sin(M_PI*H)*(pow(sin(M_PI*f),2));
        s/=pow((2*M_PI),(2*H+1));

        return s;
    }
}

/* It might be better to call this 'expected energy in wavelet band'
 * In the case where it's not just returning a calculated value, it's doing a 
 * numerical integration over our approximate spectral density function
 *
 * The inputs appear to be H the noise characteristic, wsteps the number of 
 * wavelet steps in the transform, quadraturesteps the numerical integration grid parameter, 
 * and i the number of the wavelet coefficient to be calculated)
 * I've no idea where these formulae come from*/
static double IntegratedSpectralDensity(double H, int wsteps, int quadraturesteps, int i)
{
    if(H==0.0){
        if (i==0){
            return 1-pow(2,wsteps)/M_PI*sin(M_PI/pow(2,wsteps));
        }else{
            double x=pow(2,wsteps+1-i);
            return 1-(x/M_PI)*sin(2*M_PI/x)+(x/M_PI)*sin(M_PI/x);
        }
    } else if (H==1.0){
        if(i==0){
            return pow(2,wsteps);
        }else{
            return 1e-50;
        }
    } else {
        if(i==0){
            return tgamma(2*H+1)*sin(M_PI*H)/pow(2*M_PI,2*H-1)/(1-H)*pow(2,(wsteps+1)*(2*H-1)-1);
        } else {
            /*perform a numerical integration by trapezium rule*/
            double scale,xold,xnew,yold,ynew,sum;
            scale=pow(2,wsteps+2-i);
            xold=1.0;
            yold=SpectralDensityFunction(xold/scale,H);
            sum=0.0;
            for(int j=1;j<=quadraturesteps;j++){
                xnew=(1+(double)j/quadraturesteps);
                ynew=SpectralDensityFunction(xnew/scale,H);
                sum+=(xnew-xold)*(ynew+yold);
                xold=xnew;
                yold=ynew;
            }
            return sum/2.0;
        }
    }
}

/*Code to perform the wavelet transform. There would be a good case for these functions having their own file, if it were necessary
 *to have another wavelet library in C. As it is they're included with the rest of the code so as not to complicate the interface.
 *The wavelet algorithms are cribbed and specialized from WaveLab, a MATLAB library for wavelet transforms.*/

/*Number theoretic modulus*/
static int mod(int a,int b)
{
    while(a<0) a+=b;
    return a%b;
}

/* Convolution of a periodic function with a filter in either direction
 * iconv and aconv are names for the two directions, and are cribbed from the WaveLab wavelet library which the MATLAB code used.*/
static void convolve( const double* signal, double* result, int signallen, const double* filter, int filterlen, bool sense )
{
    for(int i=0;i<signallen;i++)
    {
        result[i]=0;
        for (int j=0;j<filterlen;j++)
        {
            if(sense){
                result[i]+=filter[j]*signal[mod(i-j,signallen)];
            }else{
                result[i]+=filter[j]*signal[mod(i+j,signallen)];
            }
        }
    }
}
static void iconv( const double* signal, double* result, int signallen, const double* filter, int filterlen)
{
    convolve(signal, result, signallen, filter, filterlen, true);
}
static void aconv( const double* signal, double* result, int signallen, const double* filter, int filterlen)
{
    convolve(signal, result, signallen, filter, filterlen, false);
}

static void everyotherelementof(const double *d, int lend, double*result, int* lenresult, bool odds)
{
    int index;
    int count=0;
    if(odds) index=0; else index=1;
    for(int i=0; i<lend; i++)
    {
        if(index==0){
            index=1;
            result[count++]=d[i];
        } else {
            index=0;
        }
    }
    *lenresult=count;
}
static void oddelements(const double *d, int lend, double *result, int* lenresult)
{
    everyotherelementof(d,lend,result,lenresult,true);
}
static void evenelements(const double *d, int lend, double *result, int* lenresult)
{
    everyotherelementof(d,lend,result,lenresult,false);
}

static void DownSampleLow(const double* x, int lenx, const double* qmf, int lenqmf, double* result, int* lenresult)
{
    double intermediate[MAX_SIGNAL_LENGTH];
    aconv( x, intermediate, lenx, qmf, lenqmf);
    oddelements(intermediate, lenx, result, lenresult);
}

static void MirrorFilter(const double* qmf, double* mirrorqmf, int lenqmf)
{
    for(int i=0; i<lenqmf; i++)
    {
        if(i%2==0){
            mirrorqmf[i]=qmf[i];
        }else{
            mirrorqmf[i]=-qmf[i];
        }
    }
}

static void DownSampleHigh(const double* x, int lenx, const double* qmf, int lenqmf, double* result, int* lenresult)
{
    double intermediate[MAX_SIGNAL_LENGTH];
    double mirrorqmf[MAX_SIGNAL_LENGTH];
    MirrorFilter(qmf,mirrorqmf,lenqmf);
    iconv( x, intermediate, lenx, mirrorqmf, lenqmf);
    evenelements(intermediate, lenx, result, lenresult);
}

/*i.e. 1024 contains 10 'dyads' because it can be halved ten times.*/
static int dyadlength(int x)
{
    return ((int)((log(x)/log(2))+0.5));
}

static void copy(const double* a, double*b, int lena)
{
    for(int i=0; i<lena; i++)
    {
        b[i]=a[i];
    }
}


/*Uses the above helper functions to perform the (periodic) Wavelet Transform with a 'Quadrature Mirror Filter' qmf.*/
static void PeriodizedOrthogonalWaveletTransform(const double* x, int lenx, double* result, int L, const double* qmf, int lenqmf)
{
    double alfa[MAX_SIGNAL_LENGTH];
    double beta[MAX_SIGNAL_LENGTH];
    double temp[MAX_SIGNAL_LENGTH];
    int lenalfa,lenbeta,lentemp;

    copy(x,beta,lenx);
    lenbeta=lenx;
    int dyadbegin=lenx/2;
    for(int j=dyadlength(lenx)-1;j>=L;j--){
        
        DownSampleHigh(beta, lenbeta, qmf, lenqmf, alfa, &lenalfa);
        copy(alfa,&result[dyadbegin],lenalfa);
        dyadbegin/=2;
        DownSampleLow(beta, lenbeta, qmf, lenqmf, temp, &lentemp);
        copy(temp,beta,lentemp);
        lenbeta=lentemp;
    }
    copy(beta,result,lenbeta);
}

/*A partial application specializing the above function to the Daubechies 4 case we're actually using.*/
void PeriodizedDaubechies4WaveletTransform(const double* x, int lenx, double* result, int L)
{
    PeriodizedOrthogonalWaveletTransform(x,lenx,result,L,Daubechies4,Daubechies4len); 
}


/************End of Wavelet Transform Code**************************************/

static double squaresum(const double* x, int start, int end)
{
    double sum=0;
    for(int i=start; i<end; i++){
        sum+=x[i]*x[i];
    }
    return sum;
}

/* Goes through the various scale bands in a signal which has been (partially) wavelet transformed, calculating
 * the square sum of each band. This has an energy-at-scale feel to it. Voichita called it SE, which may or may not 
 * stand for Spectral Energy. At any rate the small scales are making a much greater contribution than the 
 * large ones, just by weight of coefficients getting summed.*/
static void calculateSE(const double* waveletcoefficients, int lenwavec, int waveletsteps, double* SE, int* lenSE)
{
    *lenSE=0;
    int N=lenwavec;
    while(waveletsteps>=1)
    {
        SE[waveletsteps-1]=squaresum(waveletcoefficients,N/2,N);
        waveletsteps-=1;
        N=N/2;
        *lenSE+=1;
    }
}

static double innerproduct(double*a, double*b, int lenab)
{
    double sum=0;
    for(int i=0; i<lenab; i++){
        sum+=a[i]*b[i];
    }
    return sum;
}

static double sumaoverb(double*a, double*b, int lenab)
{
    double sum=0;
    for(int i=0; i<lenab; i++){
        sum+=a[i]/b[i];
    }
    return sum;
}

static void vectoraplusboverc( 
        double* a, int lena,
        double* b, int lenb,
        double c,
        double* result, int* lenresult)
{
    assert(lena=lenb);
    for(int i=0; i<lena; i++)
    {
        result[i]=a[i]+b[i]/c;
    }
    *lenresult=lena;
}


static int minindex(double *a, int lena)
{
    double minimum=a[0];
    int minindex=0;
    for(int i=0; i<lena; i++)
    {
        if(a[i]<=minimum){
            minindex=i;
            minimum=a[i];
        }
    }
    return minindex;
}


/*This is the hairy and difficult part of the algorithm.
 * */
static void WaveletMaximumLikelihoodEstimator(
        const double* x, int lenx,    //the signal to be analysed
        int waveletsteps,             //the depth of the wavelet transform to use.
        const double HurstExponents[], int lenHurstExponents, //an array of candidate Hs to select from.
        double ScaleVariances[][MAX_WAVELETSTEPS+1], int lenScaleVariances, //associated expected wavelet spectral energies
        int I, //the HurstExponent to use as the initial guess for the iteration procedure.
        double* H, double* sigma) //return values: estimated H and sigma.
{

    int N=lenx;
    int J=dyadlength(lenx);
    int j0=J-waveletsteps;

    /* Perform wavelet transform on data. j0 is something like 'biggest wavelet scale not to be transformed'.
     * i.e. a bit the opposite of 'do this many steps'. 
     * I don't know why it's done this way, but that's how WaveLab does it so I've stuck with it.*/
    double waveletcoefficients[MAX_SIGNAL_LENGTH];
    PeriodizedDaubechies4WaveletTransform(x,lenx,waveletcoefficients, j0);

    /*Now calculate the thing which may be analogous to the spectral energy*/
    double SE[MAX_WAVELETSTEPS]; int lenSE;
    calculateSE(waveletcoefficients, lenx, waveletsteps, SE, &lenSE);

    /*these appear to be the number of coefficients in each wavelet band.*/
    double m[MAX_WAVELETSTEPS]; int lenm=0;
    for (int i=j0; i<J; i++){
        m[i-j0]=pow(2,i);
        lenm++;
    }

    /*Just the scale variances with the top coefficient (DC current value/average?) missing*/
    double TruncatedScaleVariances[MAX_HSTEPS+1][MAX_WAVELETSTEPS]; int lenTruncatedScaleVariances;
    for(int i=0; i<lenHurstExponents; i++){
        for(int j=1; j<lenScaleVariances; j++){
            TruncatedScaleVariances[i][j-1]=ScaleVariances[i][j];
        }
    }
    lenTruncatedScaleVariances=lenScaleVariances-1;

    /*Take logs*/
    double LogTruncatedScaleVariances[MAX_HSTEPS+1][MAX_WAVELETSTEPS]; int lenLogTScaleVariances;
    for(int i=0; i<lenHurstExponents; i++){
        for(int j=0; j<lenTruncatedScaleVariances; j++){
            LogTruncatedScaleVariances[i][j]=log(TruncatedScaleVariances[i][j]);
        }
    }
    lenLogTScaleVariances=lenTruncatedScaleVariances;

    /*reduce 2d array to 1d array by multiplying by the m-vector from earlier.*/
    double mLogTruncatedScaleVariances[MAX_HSTEPS+1]; int lenmLogTruncatedScaleVariances;
    {
        assert(lenLogTScaleVariances==lenm);
        for(int i=0; i<lenHurstExponents; i++){
            mLogTruncatedScaleVariances[i]=innerproduct(LogTruncatedScaleVariances[i], m, lenm);
        }
        lenmLogTruncatedScaleVariances=lenHurstExponents;
    }

    /*similar, but multiplying SE by one over the 'Scale Variances'*/
    double SEoneoverTruncatedScaleVariances[MAX_HSTEPS+1]; int lenSEoneoverTruncatedScaleVariances;
    {
        assert(lenSE==lenTruncatedScaleVariances);
        for(int i=0; i<lenHurstExponents; i++){
            SEoneoverTruncatedScaleVariances[i]=sumaoverb(SE, TruncatedScaleVariances[i],lenSE);
        }
        lenSEoneoverTruncatedScaleVariances=lenHurstExponents;
    }

    double wsize=(N-N/pow(2,waveletsteps));

    /* Now setting up an iterative improvement scheme. I, the index of the guess 
     * at the Hurst Exponent, is the variable being iterated on. The iteration 
     * stops when the corresponding estimate of sigma moves less than the tolerance.
     *
     * We repeatedly calculate the 'log likelihood', find the I that minimizes this, 
     * then recalculate sigma on the basis of the new I.
     * */

    double* S = TruncatedScaleVariances[I]; int lenS=lenTruncatedScaleVariances;

    assert(lenS=lenSE);
    double signew2 = sumaoverb(SE,S,lenS)/wsize;

    int it=1;
    double sigold2;
    do{ 
        sigold2=signew2;

        double LogLikelihood[MAX_HSTEPS+1]; int lenLogLikelihood;
        vectoraplusboverc(
                mLogTruncatedScaleVariances,
                lenmLogTruncatedScaleVariances,
                SEoneoverTruncatedScaleVariances,
                lenSEoneoverTruncatedScaleVariances,
                signew2,
                LogLikelihood, &lenLogLikelihood);

        I=minindex(LogLikelihood, lenLogLikelihood);

        S=TruncatedScaleVariances[I];
        signew2 = sumaoverb(SE,S,lenS)/wsize;

        it++; assert(it<=100); /*Stop if iteration is not converging*/
    } while(fabs(sigold2-signew2) > TOLERANCE);

    *H = HurstExponents[I];
    *sigma=sqrt(signew2);
}

/* Morally these variables should be local to VoichitaNumber, but because they're so expensive to calculate
 * I've made them global so that we can do them only once for multiple similar calls. An ugly hack. The function
 * that calculates them should be properly memoized instead for elegance, but that's bound to be bug prone in C.*/
double HurstExponents[MAX_HSTEPS+1];  //Candidates for H
double ScaleVariances[MAX_HSTEPS+1][MAX_WAVELETSTEPS+1]; //Calculated 'spectral densities' for corresponding noise.
int lenHurstExponents;
int lenScaleVariances;
/* Set up the tables of wavelet-spectral density coefficients. 
 * These are global so that they can be cached between similar calls */
void SetUpScaleVariances(int hsteps, int waveletsteps)
{
    /*Poor man's memoisation. This function is expensive, so if it's called twice running with 
     * the same parameters, don't bother doing all the work again.*/
    static bool calledbefore=false;
    static int oldhsteps;
    static int oldwaveletsteps;

    if (calledbefore && (oldhsteps==hsteps) && (oldwaveletsteps==waveletsteps)) return;

    calledbefore=true;
    oldhsteps=hsteps;
    oldwaveletsteps=waveletsteps;
    /*End of memoisation code*/


    for(int i=0; i<=hsteps; i++)
    {
        HurstExponents[i]=((double)i)/hsteps;
    }
    lenHurstExponents=hsteps+1;

    for(int i=0; i<=hsteps; i++){
        for(int j=0; j<=waveletsteps; j++){
            ScaleVariances[i][j]=IntegratedSpectralDensity(HurstExponents[i],waveletsteps, QUADRATURESTEPS, j);
        }
    }
    lenScaleVariances=waveletsteps+1;
}

/* The main function of the file. Takes as input a signal assumed to be fractional Gaussian noise, 
 * the number of wavelet scales to analyse it over, and a parameter for the resolution of the hurst 
 * exponent estimation, together with an initial guess. 
 * Returns estimates of H and sigma, the noise parameters.*/
void VoichitaNumber(const double* x, int lenx, int waveletsteps, int hsteps, double initialguess, double* H, double* sigma)
{
    
    assert(lenx<=MAX_SIGNAL_LENGTH);        //statically allocated arrays must be of a certain size.
    assert(hsteps<=MAX_HSTEPS);             //Just increasing the constants should be safe if you need
    assert(waveletsteps<=MAX_WAVELETSTEPS); //bigger numbers.

    assert(pow(2,dyadlength(lenx))==lenx); //Signal length must be exact power of two.

    SetUpScaleVariances(hsteps, waveletsteps);

    /*the index of the (closest candidate H to) the initial guess*/
    int INI= (int)(hsteps*initialguess+0.5);

    WaveletMaximumLikelihoodEstimator(
            x,lenx,
            waveletsteps,
            HurstExponents, lenHurstExponents, 
            ScaleVariances, lenScaleVariances,
            INI,
            H, sigma);
}



/*Everything below this line is for tests during development*/

#if 0
static void pi(int a)
{
    printf("!!%d\n",a);
}

static void pid(double a)
{
    printf("!!%f\n",a);
}

static void piad(double* a, int lena)
{
    printf("!");
    for(int i=0; i<lena; i++)
    {
        printf("%f:",a[i]);
    }
    printf("\n");
}

static void pia2d(double a[][MAX_WAVELETSTEPS], int lena, int len2a)
{
    printf("!");
    for(int i=0; i<lena; i++)
    {
        printf("!");
        for(int j=0; j<len2a; j++){
            printf("%f:",a[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

static double TrapeziumIntegration(int n, const double x[], const double y[]){
    double s=0.0;
    for(int i=1; i<n; i++){
        s+=(x[i]-x[i-1])*(y[i]+y[i-1]);
    }
    return s/2.0;
}

static void check(double a, double b)
{
    printf ("%f==%f\n", a, b);
    if(fabs(a-b)>1.0e-10){
        printf("FAILED!! difference %.20f\n",a-b);
    }
}

static void checkarray(const double * a, const double *b, int len)
{
    for(int i=0;i<len;i++)
    {
        printf ("%f==%f:", a[i], b[i]);
        if(fabs(a[i]-b[i])>1.0e-10){
            printf("FAILED!! difference %.20f\n",a[i]-b[i]);
        }
    }
    printf("\n");
}



/*When modifying rename this to main to test the internal functions*/
static int main(void){
    {
     check(SpectralDensityFunction(0.26,0.2367), 1.1067908939278011);
     const double x[]={0,1,2,2.5}, y[]={1,2,2,4};
     check(TrapeziumIntegration(4,x,y),5.0);
     check(IntegratedSpectralDensity(0.33,8,20,4),0.40566234961045133);
    }

    {
     const double filter[]={100,10,1}, signal[]={1,2,3,4,5};
     double result[]={0,0,0,0,0};
     double expectedresult1[]={154,215,321,432,543};
     double expectedresult2[]={123,234,345,451,512};
     iconv(signal, result, 5, filter, 3);
     checkarray(result,expectedresult1,5);
     aconv(signal, result, 5, filter, 3);
     checkarray(result, expectedresult2, 5);
    }
    {
     const double stuff[]={1,2,3,4,5,6,7};
     const double expectedresult1[]={1,3,5,7};
     const double expectedresult2[]={2,4,6};
     double result[]={0,0,0,0,0,0,0};
     int resultlen;
     oddelements(stuff,7,result,&resultlen);
     check(resultlen,4);
     checkarray(result,expectedresult1,4);
     evenelements(stuff,7,result,&resultlen);
     check(resultlen,3);
     checkarray(result, expectedresult2, 3);
    }

    {
        double mirror[]={0,0,0,0,0,0};
        const double MirrorDaubechies4[] = { .482962913145, -.836516303738, .224143868042,  .129409522551};
        MirrorFilter(Daubechies4,mirror,Daubechies4len);
        checkarray(mirror,MirrorDaubechies4,Daubechies4len);
    }

    {
        const double stuff[]={7,2,4,8,34,1.3,2};
        double result[]={0,0,0,0,0,0,0};
        int lenresult=7;
        {
            DownSampleLow(stuff, 7, Daubechies4, Daubechies4len, result, &lenresult);
            const double expectedresult[]={4.9150722912509996, 16.0766412165957, 17.0506313200164, 6.7521895983360007};
            check(lenresult,4);
            checkarray(result, expectedresult,4);
        }
        
        {
            DownSampleHigh(stuff, 7, Daubechies4, Daubechies4len, result, &lenresult);
            const double expectedresult[]={-4.2731681844757006, 1.8717924841490001, -25.502913505463503};
            check(lenresult,3);
            checkarray(result, expectedresult,3);
        }
    }
    {
        const double signal[]={3,2,3.7,24,25,67,79,8};
        const double expectedresult[]={3.5355339059327373, 19.586857838867363, 65.053823869162372, 61.518289963229634, -0.70710678118654746, 14.354267658086913, 29.698484809834998, -50.204581464244868};
        const double Haar[]={1/sqrt(2),1/sqrt(2)};
        double result[MAX_SIGNAL_LENGTH];

        PeriodizedOrthogonalWaveletTransform(signal, 8, result, 2, Haar, 2);

        checkarray(result, expectedresult,8);

    }
    {
        const double signal[] = {1,2,3,4,5,6,7,8};
        const double expectedresult[] = { 2.3107890345429998, 5.1392161592910002, 7.9676432840390001, 10.038195644859002, 2.8284271247450001, 1.0001166561579566e-12, 9.999778782798785e-13, 9.9964481137249095e-13};
        double result[MAX_SIGNAL_LENGTH];
        int L=2;
        PeriodizedDaubechies4WaveletTransform(signal, 8, result, L);

        checkarray(result,expectedresult,8);
    }

    {
        const double signal[]={1,2,3,4,5,6,7,8};
        double H, sigma;
        VoichitaNumber(signal,8,2,10,0.5,&H, &sigma);
        check(H,0.7);
        pid( sigma);
    }
    return 0;
}

#endif


