PROGRAM=linkedlist

run: $(PROGRAM)
	./$(PROGRAM)

$(PROGRAM): $(PROGRAM).c
	gcc $(PROGRAM).c -o $(PROGRAM)
