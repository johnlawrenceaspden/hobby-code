run: jerry
	./jerry

jerry: jerry.c
	gcc jerry.c -o jerry
