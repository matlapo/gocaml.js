const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe.skip('newphew92', () => {
    testDirectory('programs/extra/past-teams/newphew92/programs', 'parse', {
        validity: VALID
    });
});
