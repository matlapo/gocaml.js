const { testDirectory, VALID, INVALID } = require('./util');

testDirectory('grading/3-semantics+codegen/valid', 'codegen', {
    ignore: [
        '8-9-return-array.go', // Typechecker
        '8-9-return-slice.go', // Typechecker
        '9-6-callexpr-blank.go', // Weeder/Typechecker
        '9-6-callexpr.go', // Float precision
    ]
});
//testDirectory('grading/3-extra+codegen/valid', 'codegen');
