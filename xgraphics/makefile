run: run2

run1: simplesquares
	./$< -root

run2: colouredwindow
	./$< -root

clean: 

%: %.c
	gcc -o $@ $< -lX11

#colouredwindow: colouredwindow.c
#	gcc -o colouredwindow colouredwindow.c -lX11


