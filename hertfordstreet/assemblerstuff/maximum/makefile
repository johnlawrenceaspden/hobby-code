all: run maximuminc.s

run: maximum maximuminc
	-./maximuminc
	-./maximum

maximum: maximum.o
	ld maximum.o -o maximum

maximum.o: maximum.s
	as --gstabs maximum.s -o maximum.o


maximuminc: maximuminc.c
	gcc maximuminc.c -o maximuminc

maximuminc.s: maximuminc.c
	gcc -O0 -S maximuminc.c


clean:
	-rm maximum.o maximum *~ maximuminc.s maximuminc
