let prelude = "
const isObject = (value) => {
  const type = typeof value;
  return value != null && (type == 'object' || type == 'function');
}

const format = (arg) => {
  if (Array.isArray(arg)) {
    return `[${arg.map(format).join(' ')}]`;
  } else if (isObject(arg)) {
    return format(arg.values());
  }
  return arg.toString();
}

const print = (...args) => {
  process.stdout.write(args.map(format).join(''));
};

const println = (...args) => {
  process.stdout.write(args.map(format).join(' ') + '\\n');
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
