default: sing1.view

%.view: %.pdf
	evince $<
%.pdf: %.tex
	pdflatex $<

clean:
	-rm *.pdf *.aux *.log *.dvi *.out *.toc *.snm *.nav
