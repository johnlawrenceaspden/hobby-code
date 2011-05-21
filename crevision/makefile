run: stringreverse
	./stringreverse

runsimpson: simpson
	nice ./simpson

simpson: simpson.c
	cc -lm -std=c99 -o simpson simpson.c

stringreverse: stringreverse.c
	cc -o stringreverse stringreverse.c
