const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe.skip('hardik-vala', () => {
    testDirectory('programs/extra/past-teams/hardik-vala/programs', 'parse', {
        validity: VALID
    });
});
