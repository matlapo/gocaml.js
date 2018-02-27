const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe('tsani', () => {
    testDirectory('programs/extra/past-teams/tsani/programs', 'parse', {
        validity: VALID,
        ignore: [/./]
    });
});
