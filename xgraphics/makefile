run: simplest
	./$< -root

install: simplest
	rm ~/bin/simplest
	ln simplest ~/bin/simplest 

%: %.c vroot.h
	gcc -std=gnu99 -o $@ $< -lX11

SRCS := $(wildcard *.c)
PRGS := $(patsubst %.c,%,$(SRCS))

all:$(PRGS)

clean:
	rm $(PRGS)
