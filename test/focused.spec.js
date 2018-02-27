const { testDirectory, VALID, INVALID } = require('./util');

testDirectory('programs/scanner', 'scan');
testDirectory('programs/parser', 'parse');
testDirectory('programs/full', 'parse', { validity: VALID });
