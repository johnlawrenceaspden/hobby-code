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



const double       Daubechies4[] = { .482962913145,  .836516303738, .224143868042, -.129409522551};
const int    Daubechies4len = 4;

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

static int mod(int a,int b)
{
    while(a<0) a+=b;
    return a%b;
}

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

void PeriodizedDaubechies4WaveletTransform(const double* x, int lenx, double* result, int L)
{
    PeriodizedOrthogonalWaveletTransform(x,lenx,result,L,Daubechies4,Daubechies4len); 
}




static double squaresum(const double* x, int start, int end)
{
    double sum=0;
    for(int i=start; i<end; i++){
        sum+=x[i]*x[i];
    }
    return sum;
}

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

static void WaveletMaximumLikelihoodEstimator(
        const double* x, int lenx,
        int waveletsteps,
        const double HurstExponents[], int lenHurstExponents,
        double ScaleVariances[][MAX_WAVELETSTEPS+1], int lenScaleVariances,
        int I,
        double* H, double* sigma)
{

    int N=lenx;
    int J=dyadlength(lenx);
    int j0=J-waveletsteps;

    double waveletcoefficients[MAX_SIGNAL_LENGTH];

    PeriodizedDaubechies4WaveletTransform(x,lenx,waveletcoefficients, j0);

    double SE[MAX_WAVELETSTEPS]; int lenSE;
    calculateSE(waveletcoefficients, lenx, waveletsteps, SE, &lenSE);

    double m[MAX_WAVELETSTEPS]; int lenm=0;
    for (int i=j0; i<J; i++){
        m[i-j0]=pow(2,i);
        lenm++;
    }

    double TruncatedScaleVariances[MAX_HSTEPS+1][MAX_WAVELETSTEPS]; int lenTruncatedScaleVariances;
    for(int i=0; i<lenHurstExponents; i++){
        for(int j=1; j<lenScaleVariances; j++){
            TruncatedScaleVariances[i][j-1]=ScaleVariances[i][j];
        }
    }
    lenTruncatedScaleVariances=lenScaleVariances-1;

    double LogTruncatedScaleVariances[MAX_HSTEPS+1][MAX_WAVELETSTEPS]; int lenLogTScaleVariances;
    for(int i=0; i<lenHurstExponents; i++){
        for(int j=0; j<lenTruncatedScaleVariances; j++){
            LogTruncatedScaleVariances[i][j]=log(TruncatedScaleVariances[i][j]);
        }
    }
    lenLogTScaleVariances=lenTruncatedScaleVariances;

    double mLogTruncatedScaleVariances[MAX_HSTEPS+1]; int lenmLogTruncatedScaleVariances;
    {
        assert(lenLogTScaleVariances==lenm);
        for(int i=0; i<lenHurstExponents; i++){
            mLogTruncatedScaleVariances[i]=innerproduct(LogTruncatedScaleVariances[i], m, lenm);
        }
        lenmLogTruncatedScaleVariances=lenHurstExponents;
    }

    double SEoneoverTruncatedScaleVariances[MAX_HSTEPS+1]; int lenSEoneoverTruncatedScaleVariances;
    {
        assert(lenSE==lenTruncatedScaleVariances);
        for(int i=0; i<lenHurstExponents; i++){
            SEoneoverTruncatedScaleVariances[i]=sumaoverb(SE, TruncatedScaleVariances[i],lenSE);
        }
        lenSEoneoverTruncatedScaleVariances=lenHurstExponents;
    }

    double sigold2=1e308;
    const double TOL=1e-20;

    double wsize=(N-N/pow(2,waveletsteps));

    double* S = TruncatedScaleVariances[I]; int lenS=lenTruncatedScaleVariances;

    assert(lenS=lenSE);
    double signew2 = sumaoverb(SE,S,lenS)/wsize;

    int it=1;

    while(fabs(sigold2-signew2)>TOL){
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
    }

    *H = HurstExponents[I];
    *sigma=sqrt(signew2);
}

void VoichitaNumber(const double* x, int lenx, int waveletsteps, int hsteps, double initialguess, double* H, double* sigma)
{
    double HurstExponents[MAX_HSTEPS+1];
    double ScaleVariances[MAX_HSTEPS+1][MAX_WAVELETSTEPS+1];
    
    assert(lenx<MAX_SIGNAL_LENGTH);
    assert(pow(2,dyadlength(lenx))==lenx);
    assert(hsteps<MAX_HSTEPS);
    assert(waveletsteps<MAX_WAVELETSTEPS);

    for(int i=0; i<=hsteps; i++)
    {
        HurstExponents[i]=((double)i)/hsteps;
    }
    int lenHurstExponents=hsteps+1;

    for(int i=0; i<=hsteps; i++){
        for(int j=0; j<=waveletsteps; j++){
            ScaleVariances[i][j]=IntegratedSpectralDensity(HurstExponents[i],waveletsteps, QUADRATURESTEPS, j);
        }
    }
    int lenScaleVariances=waveletsteps+1;

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


