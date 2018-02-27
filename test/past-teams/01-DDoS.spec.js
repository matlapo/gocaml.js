const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe('DDoS', () => {
    testDirectory('programs/extra/past-teams/DDoS/programs', 'parse', {
        validity: VALID
    });
});
