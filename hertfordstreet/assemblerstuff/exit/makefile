run: exit
	@echo "At command prompt type ./exit to run followed by"
	@echo "echo $$""? to see the return code"
	

exit: exit.o
	ld exit.o -o exit

exit.o: exit.s
	as exit.s -o exit.o

clean:
	-rm exit.o exit *~
