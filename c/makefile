#DOOM ?= 1

# ifeq($DOOM,1)
# CFLAGS=-DDOOM
# else
# CFLAGS=
# endif

run: hello
	./hello

hello: hello.c
	cc ${CFLAGS} -o hello hello.c

clean:
	rm hello