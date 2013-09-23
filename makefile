run: knapsacks
	./knapsacks

knapsacks: knapsacks.c
	gcc -std=c99 knapsacks.c -o knapsacks
