const fs = require('fs');
const path = require('path');
const util = require('util');
const exec = util.promisify(require('child_process').exec);
const { spawnSync } = require('child_process');

const VALID = 'VALID';
const INVALID = 'INVALID';

function testDirectory(pathString, command, validity) {
    describe(path.basename(pathString), () => {
        for (const dirPath of subDirectoriesOf(pathString)) {
            const dirName = path.basename(dirPath);
            if (/invalid/i.test(dirName)) {
                validity = INVALID;
            } else if (/valid/i.test(dirName)) {
                validity = VALID;
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

exports.VALID = VALID;
exports.INVALID = INVALID;
exports.testDirectory = testDirectory;
