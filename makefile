run: efficiencyandprogress.class
	java -Xshare:off efficiencyandprogress

#apparently 'class sharing' screws the jvisualvm profiler

efficiencyandprogress.class: efficiencyandprogress.java
	javac efficiencyandprogress.java 

# run: knapsacks
# 	./knapsacks

knapsacks: knapsacks.c
	gcc -std=c99 knapsacks.c -o knapsacks
