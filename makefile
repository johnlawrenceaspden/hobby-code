PROGRAM=linkedlist

run: $(PROGRAM)
	./$(PROGRAM)

$(PROGRAM): $(PROGRAM).c
	gcc --std=gnu99 $(PROGRAM).c -o $(PROGRAM)
