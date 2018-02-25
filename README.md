# GoLite OCaml
A GoLite compiler written in OCaml (ewww C)

# Setup
## Tools
* Install OPAM
* `opam install menhir merlin ocamlbuild`
* For IDE integration, install the VSCode extension `ReasonML`
* To run the tests
  * Install node.js
  * `npm i -g jest`
# Run
## Compiler
* `./build.sh` 
* `./run.sh [mode] [filename]`
## Tests
* `jest`
* Watch mode: `jest --watchAll`
