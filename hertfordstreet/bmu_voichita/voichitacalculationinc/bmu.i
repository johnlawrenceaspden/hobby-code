%module bmu
%{
#include "bmu.h"
%}

%include "typemaps.i"

void VoichitaNumber(const double* x, int lenx, int waveletsteps, int hsteps, double initialguess, double* OUTPUT, double* OUTPUT);

%ignore VoichitaNumber;
%include "bmu.h"


%include "carrays.i"
%array_class(double, doubleArray);
