let prelude = "
const print = (...args) => {
  process.stdout.write(args.join(''));
};

const println = (...args) => {
  process.stdout.write(args.join(' ') + '\\n');
}

const _true = true;
const _false = false;

{
"

let postlude = "

_main();
}

process.exit(0);
"
