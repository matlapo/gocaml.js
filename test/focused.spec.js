const { testDirectory, VALID, INVALID } = require('./util');

testDirectory('programs/scanner', 'scan');
testDirectory('programs/parser', 'parse', {
    ignore: [
        /-too-big\.go/ // RCB: numeric-literals-too-big
    ]
});
testDirectory('programs/full', 'parse', { validity: VALID });
