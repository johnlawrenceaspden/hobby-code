compile: 
	cc square.c -o square
	cc square.c -E > square.preprocessor
	grep --color -A 10 main square.preprocessor
	./square
