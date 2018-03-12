const { spawnSync } = require('child_process');

function build() {
    spawnSync(`${__dirname}/../build.sh`, [], { cwd: `${__dirname}/..`});
};

module.exports = build;
