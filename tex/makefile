default: hellobeamer.view

%.view: %.pdf
	evince $<
%.pdf: %.tex
	pdflatex $<
