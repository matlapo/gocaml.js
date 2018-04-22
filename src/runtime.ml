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

const copy = (value) => {
  if (Array.isArray(value)) {
    return [...value];
  }

  if (typeof value == object) {
    return {...value}
  }

  else return value;
}

// https://stackoverflow.com/questions/25456013/javascript-deepequal-comparison#25456134
const deepEqual = (x, y) => {
  if (x === y) {
    return true;
  }
  else if ((typeof x == 'object' && x != null) && (typeof y == 'object' && y != null)) {
    if (Object.keys(x).length != Object.keys(y).length)
      return false;

    for (var prop in x) {
      if (y.hasOwnProperty(prop))
      {  
        if (! deepEqual(x[prop], y[prop]))
          return false;
      }
      else
        return false;
    }

    return true;
  }
  else 
    return false;
}

const append = (slice, element) => [...slice, element];

const _int = (v) => Math.floor(v);
const _rune = (v) => Math.floor(v);
const _float64 = (v) => v;
const _string = (v) => {
  if (Number.isInteger(v)) {
    return String.fromCharCode(v);
  }
  return v.toString();
}
const _bool = (v) => v

let inits = [];
const callInits = () => {
  inits.forEach(init => init())
}

const _0_true = true;
const _0_false = false;

let _;

{
"

let postlude = "
}

process.exit(0);
"
