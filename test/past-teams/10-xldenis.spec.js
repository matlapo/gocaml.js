const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe.skip('xldenis', () => {
    testDirectory('programs/extra/past-teams/xldenis/programs', 'parse', {
        validity: VALID
    });
});
