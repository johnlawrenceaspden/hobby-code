run: runduffdevice

runstringreverse: stringreverse
	./stringreverse

runsimpson: simpson
	nice ./simpson

runpuzzle: puzzle
	./puzzle

runduffdevice: duffdevice
	./duffdevice

duffdevice: duffdevice.c
	cc -o duffdevice duffdevice.c

puzzle: puzzle.c
	cc -o puzzle puzzle.c

simpson: simpson.c
	cc -lm -std=c99 -o simpson simpson.c

stringreverse: stringreverse.c
	cc -o stringreverse stringreverse.c

assembly: stringreverse.c
	gcc -c -g -Wa,-a,-ad stringreverse.c > stringreverse.lst
	gcc -c -g -Wa,-a,-ad -O3 stringreverse.c > stringreverse.O3.lst
