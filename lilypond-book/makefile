lilybook.pdf: lilybook.lytex screech-and-boink.ly
	lilypond-book --pdf lilybook.lytex
	pdflatex lilybook.tex
	evince lilybook.pdf

clean:
	-rm -rf 4a ac f1 1c
	-rm lilybook.aux lilybook.dep lilybook.log lilybook.pdf lilybook.tex
	-rm snippet-map--*.ly
	-rm snippet-names--*.ly

