const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe('newphew92', () => {
    testDirectory('programs/extra/past-teams/newphew92/programs', 'parse', {
        validity: VALID,
        ignore: [/./]
    });
});
