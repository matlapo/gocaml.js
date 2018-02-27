const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe('DDoS', () => {
    testDirectory(
        'programs/extra/past-teams/DDoS/programs/benchmarks',
        'typecheck',
        {
            validity: VALID
        }
    );

    testDirectory('programs/extra/past-teams/DDoS/programs/code', 'typecheck', {
        validity: VALID,
        ignore: [
            'code/scope_var.go' // PRCB: scope-shadow
        ]
    });

    testDirectory(
        'programs/extra/past-teams/DDoS/programs/code_extra',
        'typecheck',
        {
            validity: VALID,
            ignore: [
                'code_extra/BlankVariableValue.go', // RCB: segfault
                'code_extra/IrTest.go' // RCP: segfault
            ]
        }
    );

    describe('invalid', () => {
        testDirectory(
            'programs/extra/past-teams/DDoS/programs/invalid/syntax',
            'parse',
            {
                validity: INVALID,
                ignore: [
                    'DuplicateStructField.go', // RCB: duplicate-struct-fields
                    'RepeatedParameter.go' // RCB: repeated-parameter
                ]
            }
        );

        testDirectory(
            'programs/extra/past-teams/DDoS/programs/invalid/types',
            'typecheck',
            {
                validity: INVALID,
                ignore: ['invalid/types/NotACastType.go']
            }
        );
    });

    describe('invalid_extra', () => {
        testDirectory(
            'programs/extra/past-teams/DDoS/programs/invalid_extra/syntax',
            'parse',
            {
                validity: INVALID,
                ignore: [
                    'not_assignable.go'
                ]
            }
        );

        testDirectory(
            'programs/extra/past-teams/DDoS/programs/invalid_extra/types',
            'typecheck',
            {
                validity: INVALID
            }
        );
    });

    testDirectory(
        'programs/extra/past-teams/DDoS/programs/valid',
        'typecheck',
        {
            validity: VALID
        }
    );

    // TODO: make an option to change the test type of certain files
    testDirectory(
        'programs/extra/past-teams/DDoS/programs/valid_extra',
        'typecheck',
        {
            validity: VALID,
            ignore: [
                'syntax/Expressions.go',
                'syntax/WeirdValidMultline.go',
                'syntax/bunch.go',
                'syntax/struct.go', // RCB: segfault
                'syntax/unary.go',
                'types/UncondForNoBreak.go',
                'types/bunch.go' // PRCB: scope-shadow
            ]
        }
    );
});
