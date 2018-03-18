#!/bin/bash
#
# Usage: ./run.sh <mode> <file>
# 	mode: scan|tokens|parse|pretty|symbol|typecheck|codegen

# Check the command-line arguments are valid

if [ $# -lt 2 ]
then
	echo "Missing arguments"
	echo "Usage: $0 <mode> <file>"
	echo " + mode: scan|tokens|parse|pretty|symbol|typecheck|codegen"
	echo " + file: path to file (absolute or relative)"
	exit 1
fi

if [[ "|scan|tokens|parse|pretty|symbol|typecheck|codegen|" != *"|$1|"* ]]
then
	echo "Unknown mode \"$1\""
	echo "Usage: $0 <mode> <file>"
	echo " + mode: scan|tokens|parse|pretty|symbol|typecheck|codegen"
	echo " + file: path to file (absolute or relative)"
	exit 1

fi

# Comment and uncomment to switch between our compiler and the reference compiler
COMPILER=./src/_build/comp.byte
#COMPILER=~/golitec

# Invoke the compiler with the provided arguments: mode ($1) and file ($2)

outfile=$(echo "$2" | cut -d. -f1)

#if [ "codegen" = "$1" ];
#then
#	$COMPILER < "$2" 1> "$outfile.c"
#else
	$COMPILER "$1" "$2" < "$2"
#fi
