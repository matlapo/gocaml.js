# GoLite OCaml
A GoLite compiler written in OCaml (ewww C)

# Team Members
* Maxence Frenette (260685124)
* Maxime Plante (260685695)
* Mathieu Lapointe (260685906)

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
* `curated-past-teams` Runs tests past COMP520 teams that were checked against the reference golite compiler.
* `past-teams` Runs tests from all past COMP520 teams.

Example: `jest focused extra`
### Running only certain test files
To run only certain specific tests, use the `-t` option.
For example: `jest focused -t factorial.go`
