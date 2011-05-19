#include<stdio.h>
#include<math.h>

/*The exact answer to the gaussian integral is 1, of course*/

// sir i bear a rhyme excelling in mystic force and magic spelling
// celestial sprites elucidate all my own striving can't relate.
#define PI 3.14159265358979323846264338327

double roottwopi  = sqrt(2*PI);

int evalscount;

double gaussian(double x, double mu, double sigma)
{
  double d = (x-mu)/sigma;
  double sd;
  sd = exp(-d*d/2);
  return sd/roottwopi;
}

double normal(double x)
{
  return gaussian(x,0,1);
}

double xtothefour(double x)
{
  return x*x*x*x;
}

//(23/3)*(23/3)/2=529/18
double step(double x)
{
  evalscount++;
  if(43*x>1) return x-1.0/43;
  else return 0;
}


double exponential(double x)
{
  evalscount++;
  return exp(x);
}

double trapezium(double f(double), double a, double b)
{
  return (f(a)+f(b))/2*(b-a);
}

double simpson(double f(double), double a, double b)
{
  double avg = (a+b)/2;
  return (f(a)+4*f(avg)+f(b))/6*(b-a);
}

double simpson38(double f(double), double a, double b)
{
  double avg1 = (2*a+b)/3;
  double avg2 = (a+2*b)/3;
  return (f(a)+3*f(avg1)+3*f(avg2)+f(b))/8*(b-a);
}

double boole(double f(double), double a, double b)
{
  double avg1 = (a+3*b)/4;
  double avg2 = (2*a+2*b)/4;
  double avg3 = (3*a+b)/4;
  return (7*f(a)+32*f(avg1)+12*f(avg2)+32*f(avg3)+7*f(b))/90*(b-a);
}

double rulerecurse(double rule(double f(double), double, double), double f(double), double a, double b, int N)
{
  if (N == 0){
    return rule(f, a, b);
  } else {
    double avg = (a + b) / 2;
    --N;
    return rulerecurse(rule, f, a, avg, N)+rulerecurse(rule, f, avg, b, N);
  }
}

double adaptiverulerecurse(double rule(double f(double), double, double), double f(double), double a, double b, double allowederror)
{
  double res = rule(f,a,b);
  double avg = (a + b) / 2;
  double betterres = rule(f, a, avg)+rule(f, avg, b);
  double diff = (res-betterres);
  double absdiff;
  if (diff>0) absdiff=diff; else absdiff=-diff;
  if (absdiff<allowederror){
    return betterres;
  } else {
    return adaptiverulerecurse(rule, f, a, avg, allowederror/2)+
      adaptiverulerecurse(rule, f, avg, b, allowederror/2);
  }
}

void adaptivetrial (double rule(double f(double), double, double), double fn(double), double lower, double upper, double answer, double budget)
{
  for (int N=0;;N++){
    printf("%2d: ", N);
    evalscount=0;
    printf("%20.14f ", answer-adaptiverulerecurse(rule, fn, lower, upper, pow(0.5,N)));
    printf("(%d)\n", evalscount);
    if (evalscount > budget) break;
  }
  printf("\n");
}

void depthtrial  (double rule(double f(double), double, double), double fn(double), double lower, double upper, double answer, double budget)
{
  for (int N=0;; N++){
    printf("%2d: ", N);
    evalscount=0;
    printf("%20.14f,", answer-rulerecurse(rule, fn, lower, upper, N));
    printf("(%d)\n", evalscount);
    if (evalscount > budget) break;
  }
  printf("\n");
}


int main (void)
{
  double (*testfn)(double);
  double (*antideriv)(double);
  int lower=-8;
  int upper=8;
  int budget=100000;

  testfn = exponential;
  antideriv = exponential;
  double answer = antideriv(upper)-antideriv(lower);
  

  printf("%20.14f \n", answer);
  
  adaptivetrial(trapezium, testfn, lower, upper, answer, budget);
  adaptivetrial(boole, testfn, lower, upper, answer, budget);
  depthtrial(trapezium, testfn, lower, upper, answer, budget);
  depthtrial(boole, testfn, lower, upper, answer, budget);

}
