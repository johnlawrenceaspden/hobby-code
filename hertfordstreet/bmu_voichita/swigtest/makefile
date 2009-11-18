run: test.so swigtest.py
	python swigtest.py


test.so: test.i test.c test.h
	swig -python test.i
	gcc -fpic -c test.c test_wrap.c -I/usr/include/python2.3
	ld -shared test.o test_wrap.o -o _test.so

test.o: test.c test.h
	gcc -c -fPIC test.c


clean:
	-rm *~
	-rm *.pyc
	-rm *.o
	-rm *.so
	-rm *.so.*
	-rm test.py
	-rm test_wrap.c
