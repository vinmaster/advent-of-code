const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
util.inspect.defaultOptions.showHidden = true;
util.inspect.defaultOptions.depth = null;
util.inspect.defaultOptions.compact = true;

const log = (...args) => console.log(...args);

function sortAdapters(adapters, currentJolt = 0, result = []) {
  let diffs = [null, 0, 0, 0];
  while (adapters.length !== 0) {
    let next = adapters.shift();
    let diff = next - currentJolt;
    diffs[diff]++;
    currentJolt = next;
  }
  return diffs[1] * diffs[3];
}

function getPossibleArrangements(adapters) {
  let diffs = [null, 0, 0, 0];
  let nums = [];
  let currentJolt = 0;
  while (adapters.length !== 0) {
    let next = adapters.shift();
    let diff = next - currentJolt;
    diffs[diff]++;
    // console.log(diff);
    nums.push(diff)
    currentJolt = next;
  }

  let possible = nums.reduce((acc, val) => {
    if (val === 1) {
      acc.count++
    } else {
      if (acc.count > 1) {
        acc.out.push(acc.count)
      }
      acc.count = 0
    }
    return acc;
  }, { count: 0, out: [] }).out;

  let sum = possible.map(n => {
    if (n < 4) {
      return Math.pow(2, (n - 1))
    }
    return Math.pow(2, (n - 1)) - ((n - 3) * (n - 2) / 2)
  }).reduce((a, b) => a * b, 1)

  return sum;
}

function part1(input) {
  let adapters = input
    .trim()
    .split('\n')
    .map(Number);

  adapters = adapters.sort((a, b) => a - b);
  adapters.push(Math.max(...adapters) + 3);
  return sortAdapters(adapters);
}

function part2(input) {
  let adapters = input
    .trim()
    .split('\n')
    .map(Number);

  adapters = adapters.sort((a, b) => a - b);
  adapters.push(Math.max(...adapters) + 3);

  return getPossibleArrangements(adapters);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day10 part1:', part1(input));
log('day10 part2:', part2(input));
