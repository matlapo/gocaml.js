const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe('xldenis', () => {
    testDirectory('programs/extra/past-teams/xldenis/programs', 'parse', {
        validity: VALID,
        ignore: [/./]
    });
});
