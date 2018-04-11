const { testDirectory, VALID, INVALID } = require('./util');

testDirectory('programs/scanner', 'scan');
testDirectory('programs/parser', 'parse', {
    ignore: [
        /-too-big\.go/ // RCB: numeric-literals-too-big
    ]
});
testDirectory('programs/typecheck', 'typecheck');
// testDirectory('programs/full', 'typecheck', { validity: VALID }); // TODO: fix these tests
testDirectory('programs/codegen', 'codegen');
