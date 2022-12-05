const fs = require('fs');
const path = require('path');

function readCreates(stacksInput) {
  stacksInput = stacksInput.split('\n');
  let count = stacksInput
    .at(-1)
    .split('  ')
    .map(x => parseInt(x, 10))
    .at(-1);
  let stacks = Array.from(Array(count), () => []);

  for (let line of stacksInput.slice(0, stacksInput.length - 1)) {
    for (let i = 0; i < line.length; i += 4) {
      let crate = line.substring(i, i + 3);
      let index = i / 4;
      if (crate.trim() !== '') stacks[index].push(crate);
    }
  }

  return stacks;
}

function part1(input) {
  /** @type string[] */
  let [stacksInput, proceduresInput] = input.split('\n\n');

  let stacks = readCreates(stacksInput);

  let procedures = proceduresInput.split('\n');
  for (let p of procedures) {
    if (p.trim().length === 0) continue;
    let [_, ...str] = p.match(new RegExp('move (\\d+) from (\\d+) to (\\d+)'));
    let [move, from, to] = str.map(s => parseInt(s, 10));
    from--;
    to--;
    for (let i = 0; i < move; i++) {
      if (stacks[from].length === 0) continue;
      stacks[to].unshift(stacks[from].shift());
    }
  }
  return stacks
    .map(s => s.at(0))
    .join('')
    .split('')
    .filter(c => new RegExp('[A-Z]').test(c))
    .join('');
}

function part2(input) {
  /** @type string[] */
  let [stacksInput, proceduresInput] = input.split('\n\n');

  let stacks = readCreates(stacksInput);

  let procedures = proceduresInput.split('\n');
  for (let p of procedures) {
    if (p.trim().length === 0) continue;
    let [_, ...str] = p.match(new RegExp('move (\\d+) from (\\d+) to (\\d+)'));
    let [move, from, to] = str.map(s => parseInt(s, 10));
    from--;
    to--;
    stacks[to].unshift(...stacks[from].splice(0, move));
  }
  return stacks
    .map(s => s.at(0))
    .join('')
    .split('')
    .filter(c => new RegExp('[A-Z]').test(c))
    .join('');
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8').replaceAll('\r', '');

// input = `    [D]
// [N] [C]
// [Z] [M] [P]
//  1   2   3

// move 1 from 2 to 1
// move 3 from 1 to 3
// move 2 from 2 to 1
// move 1 from 1 to 2`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
