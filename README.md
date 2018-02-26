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
* Watch mode: `jest --watch` (Won't automatically trigger test when a file is changes)
### Running only certain test suites
To only run certain test suites, run `jest [suite names]`. The following test suites are available:
* `focused` Runs only tests that are fully written by us.
* `extra` Runs go sample programs from online sources.
* `past-teams` Runs tests from past COMP520 teams.

Example: `jest focused extra`
### Running only certain test files
To run only certain specific tests, use the `-t` option.
For example: `jest focused -t factorial.go`
