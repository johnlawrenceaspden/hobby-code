default: sing1.view

%.view: %.pdf
	evince $<
%.pdf: %.tex
	pdflatex $<

clean:
	-rm *.pdf *.aux *.log *.dvi *.out *.toc *.snm *.nav *.fls *.fdb_latexmk

# interactively, can use emacs and latexmk and evince as an instant previewing system
# cd ~/hobby-code/tex && e sing1.tex && evince sing1.pdf && latexmk -pvc -pdf sing1.tex
