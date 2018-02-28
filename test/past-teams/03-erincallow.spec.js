const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe.skip('erincallow', () => {
    testDirectory('programs/extra/past-teams/erincallow/Programs', 'parse', {
        validity: VALID
    });
});
