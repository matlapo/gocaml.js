# GoLite OCaml
A GoLite compiler written in OCaml (ewww C)

# Setup
## Tools
* Install the VSCode extension `ReasonML`
* `opam install menhir merlin ocamlbuild`
# Run
## Compiler
* `ocamlbuild comp.native -use-ocamlfind`
* `./comp.native [mode] [filename]`
## Tests
* `python3 test.py`