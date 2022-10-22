# JINX
## CS 3110 Final Project - Monopoly

## Contributors: 
| Name | NetID |
|------|-------|
| Kitil | nk528|
| Savitta | ss2849|
| Nguyen | ntv4 |
| John | ja525 |

## Testing: 
**Tests should be made at the ./test/****.ml level**
`make test` to run test cases defined within the monopoly suite

## Release:
`make zip` generates a zip file of the working directory minus exclusions defined in exclude .lst

## Installation Guidelines
 make an opam switch with `ocaml-base-compiler.4.14.0` if don't have one already <br>
 for linux Ubunu, put `opam create JINX ocaml-base-compiler.4.14.0` into CLI to have such a switch <br>
 a few packages are required to make this work, for linux ubuntu can download using following command <br>
 `opam install -y ounit2 ANSITerminal <other packages>` <br>

# Play Monopoly
 pass `dune build` into the terminal <br>
 start game by typing `dune exec bin/main.exe <your name> ` into your command line interface