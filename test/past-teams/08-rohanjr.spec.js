const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe('rohanjr', () => {
    testDirectory('programs/extra/past-teams/rohanjr/test-programs', 'parse', {
        validity: VALID
    });
});
