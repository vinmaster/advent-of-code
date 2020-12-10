const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
util.inspect.defaultOptions.showHidden = true;
util.inspect.defaultOptions.depth = null;
util.inspect.defaultOptions.compact = true;

const log = (...args) => console.log(...args);
/** @param {Array} list */
const last = (list) => list.slice(-1)[0];

/** @return {number[]} */
function patternAtPosition(pos, length) {
  let base = [0, 1, 0, -1];
  let pattern = [];
  // Get extra
  length++;
  for (let i = 0; i < length; i++) {
    let n = base[i % base.length];
    for (let j = 0; j <= pos && pattern.length < length; j++) {
      pattern.push(n);
    }
  }
  // Leave out first one
  return pattern.slice(1);
}

// Get number at ones position 
function getOnes(num) {
  return Math.abs(num % 10);
}

/** @param {number[]} list */
function phase(list) {
  let newList = [];
  for (let pos = 0; pos < list.length; pos++) {
    let pattern = patternAtPosition(pos, list.length)
    let num = list.reduce((acc, cur, i) => acc + (cur * pattern[i]), 0);
    newList.push(getOnes(num));
  }
  return newList;
}

function decodeMessage(nums, repeats, phases, digits) {
  let messageOffset = Number(nums.slice(0, 7).join(''));
  let newList = new Array(nums.length * repeats);
  for (let i = 0; i < repeats; i++) {
    for (let item = 0; item < nums.length; item++) {
      newList[(nums.length * i) + item] = nums[item];
    }
  }
  newList = newList.slice(messageOffset)
  let result = new Array(newList.length);

  for (let i = 0; i < phases; i++) {
    let count = 0;
    for (let pos = newList.length - 1; pos >= 0; pos--) {
      count = (count + newList[pos]) % 10;
      result[pos] = count;
    }
    for (let pos = 0; pos < newList.length; pos++) {
      newList[pos] = result[pos];
    }
  }
  return newList.slice(0, digits).join('');
}

function part1(input) {
  let nums = input
    .trim()
    .split('')
    .map(Number);

  let runPhaseCount = 100;
  for (let i = 0; i < runPhaseCount; i++) {
    nums = phase(nums);
  }

  return nums.slice(0, 8).join('');
}

function part2(input) {
  let nums = input
    .trim()
    .split('')
    .map(Number);

  return decodeMessage(nums, 10000, 100, 8);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day16 part1:', part1(input));
log('day16 part2:', part2(input));
