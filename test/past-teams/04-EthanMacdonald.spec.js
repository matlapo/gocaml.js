const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe('EthanMacdonald', () => {
    testDirectory(
        'programs/extra/past-teams/EthanMacdonald/programs',
        'parse',
        {
            validity: VALID,
            ignore: [/./]
        }
    );
});
