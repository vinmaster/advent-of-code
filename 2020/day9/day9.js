const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
util.inspect.defaultOptions.showHidden = true;
util.inspect.defaultOptions.depth = null;
util.inspect.defaultOptions.compact = true;
const log = (...args) => console.log(...args);

function findInvalidNum(nums, preamble) {
  let list = [];
  let map = {};
  // Setup preamble
  for (let i = 0; i < preamble; i++) {
    list.push(nums[0]);
    map[nums[0].toString()] = true;
    nums.shift();
  }

  for (let n of nums) {
    if (list.length > preamble) {
      let remove = list.shift();
      delete map[remove.toString()];
    }
    // Check n is valid
    let isValid = (n) => {
      return list.filter(m => {
        let target = n - m;
        let found = map[target.toString()];
        return found && m !== found;
      }).length > 0;
    };
    if (!isValid(n)) {
      return n;
    }

    list.push(n);
    map[n.toString()] = true;
  }
}

function sum(list) { return list.reduce((a, b) => a + b, 0); }

function findContiguousList(nums, target) {
  let list = [];
  let i = 0;
  while (i < nums.length) {
    while (sum(list) < target && i < nums.length) {
      let n = nums[i];
      list.push(n);
      i++;
    }

    if (sum(list) === target) return list;

    while (sum(list) > target) {
      list.shift();
    }
  }
}

function part1(input) {
  let nums = input
    .trim()
    .split('\n')
    .map(n => parseInt(n, 10));

  return findInvalidNum(nums, 25);
}

function part2(input) {
  let nums = input
    .trim()
    .split('\n')
    .map(n => parseInt(n, 10));

  let target = findInvalidNum([...nums], 25);
  let list = findContiguousList([...nums], target);
  return Math.min(...list) + Math.max(...list);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day9 part1:', part1(input));
log('day9 part2:', part2(input));

/*
const input = require('../input');
const log = console.log;
const ascending = (a, b) => a - b;

const findInvalidNumber = (numbers, lower = 0, upper = 25) => {
  if (upper >= numbers.length) return;

  const window = numbers.slice(lower, upper).sort(ascending);
  const current = numbers[upper];

  while (window.length >= 2) {
    const first = window[0];
    const last = window[window.length - 1];
    const sum = first + last;

    if (sum === current) break;
    if (sum < current) window.shift();
    if (sum > current) window.pop();
  }

  return window.length < 2 ? current : findInvalidNumber(numbers, ++lower, ++upper);
};

const findEncryptionWeakness = (numbers, sumTill) => {
  let lower = 0;
  let upper = 1;
  let sum = numbers[lower] + numbers[upper];

  while (upper < numbers.length) {
    if (sum === sumTill) break;

    if (sum < sumTill) {
      sum += numbers[upper + 1];
      upper++;
    }

    if (sum > sumTill) {
      sum -= numbers[lower];
      lower++;
    }
  }

  const window = numbers.slice(lower, upper);
  return Math.min(...window) + Math.max(...window);
};

const numbers = input(__dirname, './input.txt')
  .split('\n')
  .map((number) => +number);

const solutionOne = findInvalidNumber(numbers);
log(`Solution pt.1 ${solutionOne}`);

const solutionTwo = findEncryptionWeakness(numbers, solutionOne);
log(`Solution pt.2 ${solutionTwo}`);
*/
