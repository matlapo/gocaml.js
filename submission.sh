#!/bin/bash

cd programs

mkdir 1-scan+parse
mkdir 1-scan+parse/valid
mkdir 1-scan+parse/invalid
mkdir 2-typecheck
mkdir 2-typecheck/invalid
mkdir 3-semantics+codegen
mkdir 3-semantics+codegen/valid

# Valid programs
cp full/* 1-scan+parse/valid

# Invalid programs
cp scanner/1.1-code-representation/invalid/unicode.go 1-scan+parse/invalid
cp parser/1.2-keywords/invalid/func-append.go 1-scan+parse/invalid
cp parser/1.2-keywords/invalid/var-package.go 1-scan+parse/invalid
cp parser/1.4-comments/invalid/nested2.go 1-scan+parse/invalid
cp parser/1.5-literals/invalid/escape-double-quote.go 1-scan+parse/invalid
cp parser/1.5-literals/invalid/escape-single-quote.go 1-scan+parse/invalid
cp parser/1.5-literals/invalid/hex-wrong-letter.go 1-scan+parse/invalid
cp parser/1.5-literals/invalid/long-rune.go 1-scan+parse/invalid
cp parser/1.5-literals/invalid/octal-wrong-number.go 1-scan+parse/invalid
cp parser/1.6-identifiers/invalid/blank-in-assignment.go 1-scan+parse/invalid
cp parser/1.6-identifiers/invalid/blank-in-indexing.go 1-scan+parse/invalid
cp parser/1.6-identifiers/invalid/blank-in-print.go 1-scan+parse/invalid
cp parser/1.6-identifiers/invalid/start-with-number.go 1-scan+parse/invalid
cp parser/1.7-semicolons/invalid/multiline-expression.go 1-scan+parse/invalid
cp parser/2.2-package-declaration/invalid/2-packages.go 1-scan+parse/invalid
cp parser/2.2-package-declaration/invalid/empty-file.go 1-scan+parse/invalid
cp parser/2.2-package-declaration/invalid/no-package.go 1-scan+parse/invalid
cp parser/2.3-top-level-declaration/invalid/short-decl.go 1-scan+parse/invalid
cp parser/2.4-variable-declarations/invalid/unmatched-declaration.go 1-scan+parse/invalid
cp parser/2.7-types/invalid/array-variable-length.go 1-scan+parse/invalid
cp parser/2.7-types/invalid/bad-bracket-slice.go 1-scan+parse/invalid
cp parser/2.8-statements/invalid/append.go 1-scan+parse/invalid
cp parser/2.8-statements/invalid/break.go 1-scan+parse/invalid
cp parser/2.8-statements/invalid/continue.go 1-scan+parse/invalid
cp parser/2.8-statements/invalid/expression-statement.go 1-scan+parse/invalid
cp parser/2.8-statements/invalid/if-newline.go 1-scan+parse/invalid
cp parser/2.8-statements/invalid/increment-expression.go 1-scan+parse/invalid
cp parser/2.8-statements/invalid/switch.go 1-scan+parse/invalid
cp parser/2.8-statements/invalid/unmatched-assignment.go 1-scan+parse/invalid
cp parser/2.9-expressions/invalid/mismatched-parens.go 1-scan+parse/invalid

# Milestone 2
cp typecheck/1.2-types/invalid/1.2.2-different-array-size.go 2-typecheck/invalid
cp typecheck/2.3-function-declarations/invalid/2.3-wrong-return-type.go 2-typecheck/invalid
cp typecheck/3.5-short-declaration/invalid/3.5-no-new-variable.go 2-typecheck/invalid
cp typecheck/3.8-op-assignments/invalid/3.8-invalid-type2.go 2-typecheck/invalid
cp typecheck/3.9-block/invalid/3.9-out-of-scope.go 2-typecheck/invalid
cp typecheck/4.2-identifiers/invalid/4.2-undefined-symbol2.go 2-typecheck/invalid
cp typecheck/4.5-function-call/invalid/4.5-wrong-args-count.go 2-typecheck/invalid
cp typecheck/4.6-indexing/invalid/4.6-not-an-array.go 2-typecheck/invalid
cp typecheck/4.8-append/invalid/4.8-different-types.go 2-typecheck/invalid
cp typecheck/4.9-type-cast/invalid/4.9-string-to-int.go 2-typecheck/invalid

# Milestone 3
cp codegen/declarations.go 3-semantics+codegen/valid
cp codegen/identifiers.go 3-semantics+codegen/valid
cp codegen/if.go 3-semantics+codegen/valid

# Remove other folders
rm -r extra
rm -r full
rm -r parser
rm -r scanner
rm -r typecheck
rm -r codegen

cd ..
