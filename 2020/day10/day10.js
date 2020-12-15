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

/*
const input = require('../input');
const log = console.log;
const compose = (...fns) => (args) => fns.reduceRight((arg, fn) => fn(arg), args);
const ascending = (a, b) => a - b;
const includes = (values) => (value) => values.includes(value);
const last = (array) => array[array.length - 1];
const toInt = (int) => +int;

const difference = (array) => array.map((item, index, items) => items[index + 1] - item);
const filter = (predicate) => (array) => array.filter(predicate);
const partition = (predicate) => (array) =>
  array.reduce((acc, i) => (acc[predicate(i) ? 0 : 1].push(i), acc), [[], []]);

const findDifferencesOneOrThreeJolts = compose(
  partition((jolts) => jolts === 1),
  filter(includes([1, 3])),
  difference,
);

const findPossibleArrangements = (jolts) =>
  jolts.reduce(
    (jolts, jolt) => {
      const waysToReachJolt = (jolts[jolt - 1] ?? 0) + (jolts[jolt - 2] ?? 0) + (jolts[jolt - 3] ?? 0);
      return (jolts[jolt] = waysToReachJolt), jolts;
    },
    [1],
  );

const jolts = input(__dirname, './input.txt').split('\n').map(toInt).sort(ascending);
const joltsChargingOutlet = 0;
const joltsDevice = last(jolts) + 3;

const [ones, threes] = findDifferencesOneOrThreeJolts([joltsChargingOutlet, ...jolts, joltsDevice]);
log(`Solution pt.1 ${ones.length * threes.length}`);

const arrangements = last(findPossibleArrangements(jolts));
log(`Solution pt.2 ${arrangements}`);
*/