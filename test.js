const fs = require('fs');
const path = require('path');
const util = require('util');
const exec = util.promisify(require('child_process').exec);
const { spawnSync } = require('child_process');

beforeAll(() => {
    spawnSync('./build.sh');
});

testDirectory('programs/valid', (pathString) => {
    const result = spawnSync('./run.sh', ['parse', pathString]);
    const output = result.output.join('');
    
    expect(output).toMatch(/OK/);
    expect(result.status).toBe(0);
});

testDirectory('programs/parser', async (pathString) => {
    const result = spawnSync('./run.sh', ['parse', pathString]);
    const output = result.output.join('');

    expect(output).toMatch(/Error/);
    expect(result.status).toBe(1);
});

testDirectory('programs/scanner', async (pathString) => {
    const result = spawnSync('./run.sh', ['scan', pathString]);
    const output = result.output.join('');

    expect(output).toMatch(/Error/);
    expect(result.status).toBe(1);
});

function testDirectory(pathString, testFunction) {
    describe(path.basename(pathString), () => {
        for (const file of fs.readdirSync(pathString)) {
            const subPath = pathString + path.sep + file;
            if (isDir(subPath)) {
                testDirectory(subPath, testFunction);
            } else {
                it(path.basename(subPath), testFunction.bind(this, subPath));
            }
        }
    });
}

function isDir(path) {
    return fs.lstatSync(path).isDirectory();
}
