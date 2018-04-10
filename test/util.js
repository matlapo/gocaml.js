const fs = require('fs');
const path = require('path');
const util = require('util');
const exec = util.promisify(require('child_process').exec);
const { spawnSync } = require('child_process');

const VALID = 'VALID';
const INVALID = 'INVALID';

function testDirectory(pathString, command, options = {}) {
    describe(path.basename(pathString), () => {
        for (const dirPath of subDirectoriesOf(pathString)) {
            const dirName = path.basename(dirPath);
            if (/invalid/i.test(dirName)) {
                options = { ...options, validity: INVALID };
            } else if (/valid/i.test(dirName)) {
                options = { ...options, validity: VALID };
            }

            testDirectory(dirPath, command, options);
        }

        testFiles(pathString, command, options);
    });
}

function testFiles(pathString, command, options) {
    for (const filePath of subFilesOf(pathString, options.ignore)) {
        testFile(filePath, command, options);
    }
}

function testFile(pathString, command, options) {
    it(path.basename(pathString), () => {
        const result = spawnSync(
            `${__dirname}/../run.sh`,
            [command, pathString],
            { cwd: `${__dirname}/..` }
        );
        const output = result.output.join('');

        if (command === 'codegen') {
            const programPath = pathString.replace(/\.go$/, ".js");
            const programResult = spawnSync(
                `${__dirname}/../execute.sh`,
                [programPath],
                { cwd: `${__dirname}/..` }
            );
            const programOutput = programResult.output.join('');
            const fileContents = fs.readFileSync(pathString).toString();

            if (/\/\/!/.test(fileContents)) {
                expect(programResult.status).not.toBe(0);
            } else {
                const expectedOutput = fileContents
                    .match(/(?:\/\/~[^\n]*\n)*/)[0]
                    .replace(/^\/\/~(.*)$/gm, '$1');

                expect(programResult.status).toBe(0);
                expect(programOutput).toBe(expectedOutput);
            }
        } else {
            expect(options.validity).toBeDefined();

            if (options.validity === VALID) {
                expect(output).toMatch(/OK/);
                expect(result.status).toBe(0);
            } else if (options.validity === INVALID) {
                expect(output).toMatch(/Error/);
                expect(result.status).toBe(1);
            } else {
                throw new Error('unreachable');
            }
        }
    });
}

function subDirectoriesOf(directoryPath) {
    return fs
        .readdirSync(directoryPath)
        .map((p) => directoryPath + path.sep + p)
        .filter((p) => fs.lstatSync(p).isDirectory());
}

function subFilesOf(directoryPath, ignore) {
    return fs
        .readdirSync(directoryPath)
        .map((p) => directoryPath + path.sep + p)
        .filter((p) => !fs.lstatSync(p).isDirectory())
        .filter((p) => /.go$/.test(p))
        .filter((p) => {
            return (
                !ignore ||
                ignore.every((ignorePattern) => {
                    if (ignorePattern instanceof RegExp) {
                        return !ignorePattern.test(p);
                    } else if (typeof ignorePattern === 'string') {
                        return !p.includes(ignorePattern);
                    } else {
                        throw new TypeError(
                            'Ignore patterns must be RegExps or strings.'
                        );
                    }
                })
            );
        });
}

exports.VALID = VALID;
exports.INVALID = INVALID;
exports.testDirectory = testDirectory;
exports.testFiles = testFiles;
