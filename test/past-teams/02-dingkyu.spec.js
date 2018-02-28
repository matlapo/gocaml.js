const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe.skip('dingkyu9315', () => {
    testDirectory('programs/extra/past-teams/dingkyu9315/programs', 'parse', {
        validity: VALID
    });
});
