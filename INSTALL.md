**Tests should be made at the ./test/\*\***.ml level\*\*
`make test` to run test cases defined within the monopoly suite

## Release:

`make zip` generates a zip file of the working directory minus exclusions defined in exclude .lst

## Installation Guidelines

make an opam switch with `ocaml-base-compiler.4.14.0` if don't have one already <br>
for Linux or Mac system, can create switch by typing `opam create JINX ocaml-base-compiler.4.14.0` <br>
a few packages are required to make this work, for Linux and Mac systems, can download using following command <br>
`opam install -y ounit2 ppx_deriving_yojson core ANSITerminal` <br>

# Play Monopoly

pass `dune build` into the terminal <br>
start game by typing `make play` into the terminal
