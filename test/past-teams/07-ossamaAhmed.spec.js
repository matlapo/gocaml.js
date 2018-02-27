const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe('ossamaAhmed', () => {
    testDirectory('programs/extra/past-teams/ossamaAhmed/programs', 'parse', {
        validity: VALID,
        ignore: [/./]
    });
});
