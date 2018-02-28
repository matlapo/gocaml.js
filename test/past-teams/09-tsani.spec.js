const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe.skip('tsani', () => {
    testDirectory('programs/extra/past-teams/tsani/programs', 'parse', {
        validity: VALID
    });
});
