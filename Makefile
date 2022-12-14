.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip:
	dune clean
	rm -f monopoly.zip
	zip -r monopoly.zip . -x@exclude.lst

clean:
	dune clean
	rm -f adventure.zip

webserver:
	cd gui && python3 -m http.server

navigate-gui:
	open http://localhost:8000

doc:
	dune build
	dune build @doc ; dune build
	dune build @doc

opendoc: doc
	@bash opendoc.sh
