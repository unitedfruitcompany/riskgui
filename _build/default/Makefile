build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

zip:
	rm -f risk.zip
	zip -r risk.zip . -x@exclude.lst

tester:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
	rm -f risk.zip