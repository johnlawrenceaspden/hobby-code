tests: all
	python2.4 voichita.py

all: bmuexample bmu.so

include bmumakefile

bmu.so: bmu.i bmu.c bmu.h
	swig -python bmu.i
	gcc -std=c99 -fpic -c bmu.c bmu_wrap.c -I/usr/include/python2.3
	ld -shared bmu.o bmu_wrap.o -o _bmu.so

reallyclean: clean
	-rm *.pyc
	-rm *.so
	-rm bmu.py
	-rm bmu_wrap.c
