%module test
%{
#include "test.h"
%}

%include "test.h"
%include "carrays.i"
%array_class(int, intArray);
%array_class(double, doubleArray);
