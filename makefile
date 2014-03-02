run: integral
	./integral

integral: integral.c
	gcc integral.c -o integral
