display: fib.svg factorial.svg fact-iter.svg
	eog fib.svg &
	eog fact-iter.svg &
	eog factorial.svg &

%.svg: %.dot
	dot <$*.dot -Tsvg >$*.svg

fib.dot: fibgraph.scm
	racket fibgraph.scm >fib.dot

# fact-iter.svg: fact-iter.dot
# 	dot <fact-iter.dot -Tsvg >fact-iter.svg
