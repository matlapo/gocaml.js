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

testDirectory('programs/scanner', 'scan');
testDirectory('programs/parser', 'parse');
testDirectory('programs/full', 'parse', VALID);
testDirectory('programs/extra/rosettacode', 'parse', VALID);

function testDirectory(pathString, command, validity) {
    describe(path.basename(pathString), () => {
        for (const fileName of fs.readdirSync(pathString)) {
            const subPath = pathString + path.sep + fileName;
            if (isDir(subPath)) {
                if (fileName == 'valid') {
                    validity = VALID;
                } else if (fileName === 'invalid') {
                    validity = INVALID;
                }

                testDirectory(subPath, command, validity);
            } else {
                testFile(subPath, command, validity)
            }
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

function isDir(pathString) {
    return fs.lstatSync(pathString).isDirectory();
}
