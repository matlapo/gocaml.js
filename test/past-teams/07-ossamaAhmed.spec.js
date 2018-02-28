const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe.skip('ossamaAhmed', () => {
    testDirectory('programs/extra/past-teams/ossamaAhmed/programs', 'parse', {
        validity: VALID
    });
});
