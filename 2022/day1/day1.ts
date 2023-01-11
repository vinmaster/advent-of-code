// deno-lint-ignore-file prefer-const no-explicit-any

function part1(input: string) {
  return pipe(
    trim,
    curry(split)('\n\n'),
    (list: string[]) => {
      return list.map(s =>
        s
          .split('\n')
          .map(Number)
          // @ts-ignore use sum
          .sum()
      );
    },
    (list: number[]) => Math.max(...list)
  )(input);
}

function part2(input: string) {
  let lines = input.trim().split('\n\n');

  let max = lines
    // @ts-ignore use sum
    .map(person => person.split('\n').map(Number).sum())
    // @ts-ignore use sortf
    .sortf((a, b) => b - a)
    .slice(0, 3)
    .sum();

  return max;
}

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// 1000
// 2000
// 3000

// 4000

// 5000
// 6000

// 7000
// 8000
// 9000

// 10000`;

Object.defineProperty(Array.prototype, 'sortf', {
  value: function (compareFn: (a: any, b: any) => any) {
    return [].concat(this).sort(compareFn);
  },
});

Object.defineProperty(Array.prototype, 'sum', {
  value: function () {
    return this.reduce((a: number, b: number) => a + b, 0);
  },
});

console.log('part1:', part1(input));
console.log('part2:', part2(input));

function trim(s: string) {
  return s.trim();
}

function split(seperator: string, s: string) {
  return s.split(seperator);
}

function pipe(...fns: ((a: any) => any)[]) {
  return (v: any) => fns.reduce((a, f) => f(a), v);
}

function curry(fn: (...args: any[]) => any) {
  return function (...args: any[]) {
    // fn.length is number of arguments
    if (args.length >= fn.length) {
      return fn.apply(null, args); // just call fn
    }
    // bind a new function with passed arguments
    return fn.bind(null, ...args);
  };
}
