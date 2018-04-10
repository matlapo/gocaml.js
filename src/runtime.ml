let prelude = "
const print = (...args) => {
  process.stdout.write(args.join(''));
};

const println = (...args) => {
  process.stdout.write(args.join(' ') + '\\n');
}

const at = (array, index) => {
  if (index >= array.length) {
    throw new Error('Error: Array or Slice accessed out of bounds');
  }

  return array[index];
}

const append = (slice, element) => [...slice, element];

const _true = true;
const _false = false;

{
"

let postlude = "

_main();
}

process.exit(0);
"
