const { VALID, INVALID, testDirectory, testFiles } = require('../util');

describe('yiqiaowang', () => {
    testDirectory(
        'programs/extra/past-teams/yiqiaowang/programs/benchmark',
        'typecheck',
        {
            validity: VALID,
            ignore: [
                'dijkstra.go', // RCB: 2d-array-typechecking
                'knapsack.go' // RCB: 2d-array-typechecking
            ]
        }
    );

    testDirectory(
        'programs/extra/past-teams/yiqiaowang/programs/code',
        'typecheck',
        {
            validity: VALID,
            ignore: [
                'if_init_shadow.go', // PRCB: if-init-shadow
                'type_casting.go' // Potential reference compiler bug
            ]
        }
    );

    testDirectory(
        'programs/extra/past-teams/yiqiaowang/programs/invalid',
        'typecheck',
        {
            validity: INVALID,
            ignore: ['typechecker/cast1.go', 'types/cast1.go']
        }
    );

    // Ignore directory: programs/extra/past-teams/yiqiaowang/programs/tmp

    describe('valid', () => {
        testDirectory(
            'programs/extra/past-teams/yiqiaowang/programs/valid/codegen',
            'typecheck',
            {
                validity: VALID,
                ignore: [
                    'append.go',
                    'identifiers.go', // Potential reference compiler bug
                    'if_init_shadow.go', // PRCB: if-init-shadow
                    'type_casting.go' // Reference compiler bug
                ]
            }
        );
        testDirectory(
            'programs/extra/past-teams/yiqiaowang/programs/valid/syntax',
            'parse',
            { validity: VALID }
        );
        testDirectory(
            'programs/extra/past-teams/yiqiaowang/programs/valid/typechecker',
            'typecheck',
            {
                validity: VALID,
                ignore: [
                    'declarations/func_dclr_struct.go', // Reference compiler bug
                    /expressions\/cast_.*\.go/,
                    'statements/svd_multiple.go' // Reference compiler bug
                ]
            }
        );
        testFiles(
            'programs/extra/past-teams/yiqiaowang/programs/valid',
            'typecheck',
            {
                validity: VALID
            }
        );
    });
});
