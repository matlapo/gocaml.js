const { spawnSync } = require('child_process');

function build() {
    spawnSync('./build.sh');
};

module.exports = build;
