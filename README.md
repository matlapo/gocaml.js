# GoLite OCaml
A GoLite compiler written in OCaml (ewww C)

The best compiler of the history of compilers, maybe ever.

# Team Members
* Maxence Frenette (260685124)
* Maxime Plante (260685695)
* Mathieu Lapointe (260685906)

# Notes for the Grader
For our own convienience, we use a different directory structure for our test programs than what is required. Please run `./submission.sh` to copy generate the correct test folder for grading. **Please do not commit these changes as it effectively deletes most of our tests.**

Since the Trottier SOCS machine have a version of OCaml that dates from 2014, this project won't build on them. To build the project, you will need to
* Install OPAM
* `opam install menhir ocamlbuild`
* `./build.sh`

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

Example: `jest focused extra`
### Running only certain test files
To run only certain specific tests, use the `-t` option.
For example: `jest focused -t factorial.go`

# Credits
## The OCaml Batteries Included Library
Due to technical constraints, the only way to use [OCaml Batteries Included](https://github.com/ocaml-batteries-team/batteries-included) and still be able to build on the Trottier SOCS machines was to include the library in source control. We are using the BatOption module which can be found in `/src/batOption.ml`.
