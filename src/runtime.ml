let prelude = "
const print = (...args) => {
  process.stdout.write(args.join(''));
};

const println = (...args) => {
  process.stdout.write(args.join(' ') + '\\n');
}

"

let postlude = "

_main();
process.exit(0);
"
