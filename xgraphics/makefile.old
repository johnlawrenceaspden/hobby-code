run: run3


run1: simplesquares
	./$< -root

run2: colouredwindow
	./$< -root

run3: simplest
	./$< -root

clean:
	rm simplesquares colouredwindow simplest

%: %.c
	gcc -std=gnu99 -o $@ $< -lX11

#colouredwindow: colouredwindow.c
#	gcc -o colouredwindow colouredwindow.c -lX11

install: simplest
	rm ~/bin/simplest
	ln simplest ~/bin/simplest 

