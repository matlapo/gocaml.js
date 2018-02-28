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
* `past-teams` Runs tests from past COMP520 teams.

Example: `jest focused extra`
### Running only certain test files
To run only certain specific tests, use the `-t` option.
For example: `jest focused -t factorial.go`

# Credits
## Test from external sources
* Some tests were inspired from pseudocode found on Wikipedia or on other sites. The attribution is mentionned in the relevant files.
* The tests in `/programs/extra/rosettacode` were directly taken from [Rosetta Code's Go section](http://rosettacode.org/wiki/Category:Go) with minor adaptations.
* The tests in `/programs/extra/past-teams` were directly taken from Previous COMP 520 Teams. Their code wasn't copied and was included through submodules instead. Although git submodules force us to include full repositories, only some of the test programs were used to test this compiler.
  * https://github.com/DDoS/Golite/tree/6c1536a965a399ba70764f0c98c375c5f20b344d
  * https://github.com/EthanMacdonald/Compylor/tree/cc4bd30cb9465291d23a9728ab321b2937c0d071
  * https://github.com/dongkyu9315/GoLiteCppCompiler/tree/08d63c79cfeeba615db4d9432f44fd2dd038c074
  * https://github.com/erincallow/GoCompiler/tree/05c4be76fb44000d0e27ad2296c704cfd4237760
  * https://github.com/hardik-vala/gopy/tree/ceefa7749c1da245524672bd7f08d4e0948391ad
  * https://github.com/newphew92/GOcaml/tree/386125602a0e1e394a3ae194a4238d4234a1bd19
  * https://github.com/ossamaAhmed/GoLite-compiler/tree/8ddf3a12c4d98f844f98c6f6e201629753497a9a
  * https://github.com/rohanjr/golite-compiler/tree/6a938bce68ba399d0de5fc869f0f289963df1a84
  * https://github.com/tsani/goto/tree/38cba53f9a5a955872bad548cecafdc6764996a4
  * https://github.com/xldenis/MGC/tree/caeb2d72d16fbbefeb5104d7133c537fea66ba21
  * https://github.com/yiqiaowang/goLite/tree/99280564536ab8184059bf2c73e1c1de0b676612
## The OCaml Batteries Included Library
Due to technical constraints, the only way to use [OCaml Batteries Included](https://github.com/ocaml-batteries-team/batteries-included) and still be able to build on the Trottier SOCS machines was to include the library in source control. We are using the BatOption module which can be found in `/src/batOption.ml`.
