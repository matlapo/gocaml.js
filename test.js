const fs = require('fs');
const path = require('path');
const util = require('util');
const exec = util.promisify(require('child_process').exec);
const { spawnSync } = require('child_process');

const VALID = 'VALID';
const INVALID = 'INVALID';

beforeAll(() => {
    spawnSync('./build.sh');
});

// Our tests
testDirectory('programs/scanner', 'scan');
testDirectory('programs/parser', 'parse');
testDirectory('programs/full', 'parse', VALID);

// Extra tests
testDirectory('programs/extra/rosettacode', 'parse', VALID);

// Tests from past teams
if (fs.readdirSync('programs/extra/past-teams').length > 1) {
    // DDoS
    testDirectory('programs/extra/past-teams/DDoS/programs', 'parse', VALID);

    // dingkyu9315
    testDirectory(
        'programs/extra/past-teams/dingkyu9315/programs',
        'parse',
        VALID
    );

    // erincallow
    testDirectory(
        'programs/extra/past-teams/erincallow/Programs',
        'parse',
        VALID
    );

    // EthanMacdonald
    testDirectory(
        'programs/extra/past-teams/EthanMacdonald/programs',
        'parse',
        VALID
    );

    // hardik-vala
    testDirectory(
        'programs/extra/past-teams/hardik-vala/programs',
        'parse',
        VALID
    );

    // newphew92
    testDirectory(
        'programs/extra/past-teams/newphew92/programs',
        'parse',
        VALID
    );

    // ossamaAhmed
    testDirectory(
        'programs/extra/past-teams/ossamaAhmed/programs',
        'parse',
        VALID
    );

    // rohanjr
    testDirectory(
        'programs/extra/past-teams/rohanjr/test-programs',
        'parse',
        VALID
    );

    // tsani
    testDirectory(
        'programs/extra/past-teams/tsani/programs',
        'parse',
        VALID
    );

    // xldenis
    testDirectory(
        'programs/extra/past-teams/xldenis/programs',
        'parse',
        VALID
    );

    // yiqiaowang
    testDirectory(
        'programs/extra/past-teams/yiqiaowang/programs',
        'parse',
        VALID
    );
}

function testDirectory(pathString, command, validity) {
    describe(path.basename(pathString), () => {
        for (const dirPath of subDirectoriesOf(pathString)) {
            const dirName = path.basename(dirPath);
            if (/valid/i.test(dirName)) {
                validity = VALID;
            } else if (/invalid/i.test(dirName)) {
                validity = INVALID;
            }

            testDirectory(dirPath, command, validity);
        }

        for (const filePath of subFilesOf(pathString)) {
            testFile(filePath, command, validity);
        }
    });
}

function testFile(pathString, command, validity) {
    it(path.basename(pathString), () => {
        expect(validity).toBeDefined();

        const result = spawnSync('./run.sh', [command, pathString]);
        const output = result.output.join('');

        if (validity === VALID) {
            expect(output).toMatch(/OK/);
            expect(result.status).toBe(0);
        } else if (validity === INVALID) {
            expect(output).toMatch(/Error/);
            expect(result.status).toBe(1);
        } else {
            throw new Error('unreachable');
        }
    });
}

function subDirectoriesOf(directoryPath) {
    return fs
        .readdirSync(directoryPath)
        .map((p) => directoryPath + path.sep + p)
        .filter((p) => fs.lstatSync(p).isDirectory());
}

function subFilesOf(directoryPath) {
    return fs
        .readdirSync(directoryPath)
        .map((p) => directoryPath + path.sep + p)
        .filter((p) => !fs.lstatSync(p).isDirectory())
        .filter((p) => /.go$/.test(p));
}
