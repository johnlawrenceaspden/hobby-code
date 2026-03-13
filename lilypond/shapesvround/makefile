# Makefile for compiling LilyPond files and viewing PDFs

# List of LilyPond files
LILYPOND_FILES := shapes.ly round.ly

# Corresponding PDF files (replace .ly with .pdf)
PDF_FILES := $(LILYPOND_FILES:.ly=.pdf)

# Default target: build PDFs
all: $(PDF_FILES)

# Rule to compile .ly to .pdf
%.pdf: %.ly
	lilypond $<

# View PDFs with Evince
view: $(PDF_FILES)
	evince shapes.pdf &
	evince round.pdf &

# Clean generated files
clean:
	rm -f *.pdf *.ps *.midi *.ly~ *.pdf~

.PHONY: all view clean

