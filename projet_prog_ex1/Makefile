all: test_my_list rapport.pdf

test_my_list: my_list.ml my_list.mli test_list.ml
	ocamlc my_list.mli my_list.ml test_list.ml -o test_my_list

rapport.pdf: rapport.tex
	pdflatex -shell-escape rapport.tex

clean:
	rm -rf *.cmi *.cmo *.cmx *.aux *~ *.o *.log *.dvi _minted-rapport

propper: clean
	rm rapport.pdf test_my_list
