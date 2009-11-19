all: ocaml-graphics grtest1 grtest2


ocaml-graphics:
	ocamlmktop -o ocaml-graphics graphics.cma

grtest1:
	ocamlc graphics.cma grtest1.ml -o grtest1

grtest2:
	ocamlc graphics.cma grtest2.ml -o grtest2

clean:
	-rm *.cmi
	-rm *.cmo
	-rm ocaml-graphics
	-rm grtest1
	-rm grtest2
	-rm *~
