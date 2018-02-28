const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe.skip('EthanMacdonald', () => {
    testDirectory(
        'programs/extra/past-teams/EthanMacdonald/programs',
        'parse',
        {
            validity: VALID
        }
    );
});
