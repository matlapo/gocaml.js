#/bin/bash
#
# Build the compiler
#
# You MUST replace the following commands with the commands for building your compiler

#cd src
#ocamlbuild comp.native
make clean -C ./src
make -C ./src
#cd ..


