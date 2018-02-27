const { VALID, INVALID, testDirectory, testFiles } = require('./util');

describe('DDoS', () => {
    testDirectory('programs/extra/past-teams/DDoS/programs', 'parse', {
        validity: VALID
    });
});

describe('dingkyu9315', () => {
    testDirectory('programs/extra/past-teams/dingkyu9315/programs', 'parse', {
        validity: VALID
    });
});

describe('erincallow', () => {
    testDirectory('programs/extra/past-teams/erincallow/Programs', 'parse', {
        validity: VALID
    });
});

describe('EthanMacdonald', () => {
    testDirectory(
        'programs/extra/past-teams/EthanMacdonald/programs',
        'parse',
        { validity: VALID }
    );
});

describe('hardik-vala', () => {
    testDirectory('programs/extra/past-teams/hardik-vala/programs', 'parse', {
        validity: VALID
    });
});

describe('newphew92', () => {
    testDirectory('programs/extra/past-teams/newphew92/programs', 'parse', {
        validity: VALID
    });
});

describe('ossamaAhmed', () => {
    testDirectory('programs/extra/past-teams/ossamaAhmed/programs', 'parse', {
        validity: VALID
    });
});

describe('rohanjr', () => {
    testDirectory('programs/extra/past-teams/rohanjr/test-programs', 'parse', {
        validity: VALID
    });
});

describe('tsani', () => {
    testDirectory('programs/extra/past-teams/tsani/programs', 'parse', {
        validity: VALID
    });
});

describe('xldenis', () => {
    testDirectory('programs/extra/past-teams/xldenis/programs', 'parse', {
        validity: VALID
    });
});

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
