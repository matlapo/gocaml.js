# Gocaml.js
A GoLite compiler written in OCaml

The best compiler of the history of compilers, maybe ever. Started as an academic project in a graduate compiler design course (COMP 520) taught at McGill University.

# Setup
## Tools
* Install OPAM
* `opam install menhir merlin ocamlbuild batteries`
* For IDE integration, install the VSCode extension `ReasonML`
* Install Node.js
* To run the tests
  * `npm i -g jest`
* To prettify generated program output
  * `npm i -g pretter`

# Run
## Compiler
* `./build.sh` 
* `./run.sh [mode] [filename]`
## Tests
* `jest`
* Watch mode: `jest --watch` (Won't automatically trigger test when a file is changes)
### Running only certain test suites
To only run certain test suites, run `jest [suite names]`. The following test suites are available:
* `focused` Runs only tests that are fully written by us.
* `extra` Runs go sample programs from online sources.
## Running generated programs
* `node program.js`

Example: `jest focused extra`
### Running only certain test files
To run only certain specific tests, use the `-t` option.
For example: `jest focused -t factorial.go`
