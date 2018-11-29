lilybook.pdf: lilybook.lytex screech-and-boink.ly
	lilypond-book --pdf lilybook.lytex
	pdflatex lilybook.tex
	evince lilybook.pdf

clean:
	rm -rf 4a ac f1/ lilybook.aux lilybook.dep lilybook.log lilybook.pdf lilybook.tex snippet-map--2682974327569550602.ly snippet-names--2682974327569550602.ly

