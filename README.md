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
* `opam install menhir ocamlbuild batteries`
* `./build.sh`

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
