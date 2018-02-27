const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe('erincallow', () => {
    testDirectory('programs/extra/past-teams/erincallow/Programs', 'parse', {
        validity: VALID
    });
});
