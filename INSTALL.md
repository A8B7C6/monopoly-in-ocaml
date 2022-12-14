**Tests should be made at the ./test/\*\***.ml level\*\*
`make test` to run test cases defined within the monopoly suite

## Release:

`make zip` generates a zip file of the working directory minus exclusions defined in exclude .lst

## Installation Guidelines

make an opam switch with `ocaml-base-compiler.4.14.0` if don't have one already <br>
for Linux or Mac system, can create switch by typing `opam create JINX ocaml-base-compiler.4.14.0` <br>
a few packages are required to make this work, for Linux and Mac systems, can download using following command <br>
`opam install -y ounit2 ppx_deriving_yojson core ANSITerminal` <br>
`make doc` should be ran twice incase an error appears on the first run <br>
`make opendoc` allows you to run `make doc` and open it in your file system

# Play Monopoly

pass `dune build` into the terminal <br>
start game by typing `make play` into the terminal

## Supplemental GUI (Not part of OCaml System)

After running `make play` and initiating a game, you may additionally see the current player and their respective key statistics by hosting a webserver out of the /gui directory. The reccomended approach is to use the "Live Server" extension and serving the index.html file located in /gui.
