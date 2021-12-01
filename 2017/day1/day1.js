const fs = require('fs');
const path = require('path');
const util = require('util');

util.inspect.defaultOptions.maxArrayLength = null;
// util.inspect.defaultOptions.showHidden = true;
// util.inspect.defaultOptions.depth = null;
// util.inspect.defaultOptions.compact = true;

const log = (...args) => console.log(...args);
const sum = (acc, cur) => acc + cur;

function matchAt(list, index, value) {
  index = (index + list.length) % list.length;
  return list[index] === value;
}

function part1(input) {
  let nums = input
    .trim()
    .split('')
    .map(Number);

  return nums
    .map((val, index) => matchAt(nums, index + 1, val) ? val : null)
    .filter(val => !!val)
    .reduce(sum);
}

function part2(input) {
  let nums = input
    .trim()
    .split('')
    .map(Number);

  let halfWayAround = nums.length / 2;

  return nums
    .map((val, index) => matchAt(nums, index + halfWayAround, val) ? val : null)
    .filter(val => !!val)
    .reduce(sum, 0);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day1 part1:', part1(input));
log('day1 part2:', part2(input));
