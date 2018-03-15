#!/bin/bash

cd programs

mkdir 1-scan+parse
mkdir 1-scan+parse/valid
mkdir 1-scan+parse/invalid
mkdir 2-typecheck
mkdir 2-typecheck/invalid

# Valid programs
cp full/* 1-scan+parse/valid

# Invalid programs
cp scanner/1.1-code-representation/invalid/unicode.go 1-scan+parse/invalid
cp parser/1.2-keywords/invalid/func-append.go 1-scan+parse/invalid
cp parser/1.2-keywords/invalid/var-package.go 1-scan+parse/invalid
cp parser/1.4-comments/invalid/nested2.go 1-scan+parse/invalid
cp parser/1.5-literals/invalid/* 1-scan+parse/invalid
rm 1-scan+parse/invalid/*-too-big.go
cp parser/1.6-identifiers/invalid/* 1-scan+parse/invalid
cp parser/1.7-semicolons/invalid/* 1-scan+parse/invalid
cp parser/2.2-package-declaration/invalid/* 1-scan+parse/invalid
cp parser/2.3-top-level-declaration/invalid/* 1-scan+parse/invalid
cp parser/2.4-variable-declarations/invalid/* 1-scan+parse/invalid
cp parser/2.7-types/invalid/* 1-scan+parse/invalid
rm 1-scan+parse/invalid/bad-bracket-slice2.go
cp parser/2.8-statements/invalid/* 1-scan+parse/invalid
cp parser/2.9-expressions/invalid/* 1-scan+parse/invalid
cp milestone2/* 2-typecheck/invalid

# Remove other folders
rm -r extra
rm -r full
rm -r parser
rm -r scanner
rm -r typecheck

cd ..
