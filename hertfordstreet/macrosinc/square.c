#include <stdio.h>

//#define square(x) x*x
//#define square(x) (x*x)
#define square(x) ((x)*(x))
#define put(x) { int a = x; printf("\"%s\" = %d (%f) \n", #x, a, a);}

//void put(double a){
//    printf("%f\n",a);
//}

int main (void){
    put(square(10));
    put(square(10.0));
    put(200/square(10));
    put(square(1+1));
    int x = 2;
    put(x);
    put(square(x++));
    put(square(x++));
    put("hi");
    return(0);
}
