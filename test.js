const fs = require('fs');
const path = require('path');
const util = require('util');
const exec = util.promisify(require('child_process').exec);
const { spawnSync } = require('child_process');

beforeAll(() => {
    spawnSync('./build.sh');
});

testDirectory('programs/valid', 'parse', true);
testDirectory('programs/parser', 'parse', false);
testDirectory('programs/scanner', 'scan', false);

function testDirectory(pathString, command, valid) {
    describe(path.basename(pathString), () => {
        for (const file of fs.readdirSync(pathString)) {
            const subPath = pathString + path.sep + file;
            if (isDir(subPath)) {
                testDirectory(subPath, command, valid);
            } else {
                testFile(subPath, command, valid)
            }
        }
    });
}

function testFile(pathString, command, valid) {
    it(path.basename(pathString), () => {
        const result = spawnSync('./run.sh', [command, pathString]);
        const output = result.output.join('');
    
        if (valid) {
            expect(output).toMatch(/OK/);
            expect(result.status).toBe(0);
        } else {
            expect(output).toMatch(/Error/);
            expect(result.status).toBe(1);
        }
    });
}

function isDir(path) {
    return fs.lstatSync(path).isDirectory();
}
