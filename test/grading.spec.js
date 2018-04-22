const { testDirectory, VALID, INVALID } = require('./util');

testDirectory('grading/3-semantics+codegen/valid', 'codegen', {
    ignore: [
        '4-vardecs-init.go', // Float printing
        '8-6-shortdecstmts.go', // Float printing
        '8-7-incdecstmts.go', // Float printing
        '8-8-printstmts.go', // Parser (hex and octal) and float printing
        '8-9-return-array.go', // Typechecker
        '8-9-return-slice.go', // Typechecker
        '9-6-callexpr-blank.go', // Weeder/Typechecker
        '9-6-callexpr.go', // Float printing
        '9-8-typecastexpr.go', // Float printing
    ]
});
//testDirectory('grading/3-extra+codegen/valid', 'codegen');
