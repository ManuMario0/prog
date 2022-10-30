all:aritha rapport.pdf

aritha:x86_64.mli x86_64.ml parser.mli parser.ml lexer.mli lexer.ml translator.mli translator.ml main.ml
	ocamlopt x86_64.mli x86_64.ml lexer.mli lexer.ml parser.mli parser.ml translator.mli translator.ml main.ml -o aritha

rapport.pdf: rapport.tex
	pdflatex -shell-escape -interaction=nonstopmode rapport.tex

tests/test1 tests/test2: tests/test1.exp tests/test2.exp aritha
	./aritha tests/test1.exp
	gcc tests/test1.s -o tests/test1
	./aritha tests/test2.exp
	gcc tests/test2.s -o tests/test2

tests: tests/test1 tests/test2

clean:
	rm *.cmi *.cmx *.o *.aux *.log

propper:
	rm -r aritha rapport.pdf tests/*.s tests/test1 tests/test2
