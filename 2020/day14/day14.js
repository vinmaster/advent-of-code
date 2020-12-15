const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
// util.inspect.defaultOptions.showHidden = true;
// util.inspect.defaultOptions.depth = null;
// util.inspect.defaultOptions.compact = true;

const log = (...args) => console.log(...args);
const dec2bin = (dec) => (dec >>> 0).toString(2);
const fulldec2bin = (dec, length) => dec2bin(dec).padStart(length, '0').split('');

function applyMask(mask, value) {
  let binary = fulldec2bin(value, 36);

  for (let m of mask) {
    const { i, b } = m;
    if (b == 'X') {
      continue;
    }

    binary[i] = b.toString();
  }

  return parseInt(binary.join(''), 2);
}

function applyMaskToMemory(mask, memory) {
  let binary = fulldec2bin(memory, 36);

  for (let m of mask) {
    const { i, b } = m;
    if (b === '0') {
      continue;
    }
    binary[i] = b.toString();
  }

  let memoryTargets = [];
  const floating = mask.filter((a) => a.b == 'X');

  for (let i = 0; i < Math.pow(2, floating.length); i++) {
    let result = [...binary];

    const temp = fulldec2bin(i, floating.length);

    for (let j = 0; j < temp.length; j++) {
      const { i: index } = floating[j];
      result[index] = temp[j].toString();
    }

    memoryTargets.push(parseInt(result.join(''), 2));
  }

  return memoryTargets;
}

function part1(input) {
  let lines = input
    .trim()
    .split('\n');

  let memory = {};
  let mask;
  for (let line of lines) {
    if (line.includes('mask')) {
      // Parse mask
      let [_, str] = line.split(' = ');
      mask = str.split("").map((b, i) => {
        return { i, b };
      });
    } else {
      // Parse memory
      let [_, address, value] = line.match(/^mem\[(\d+)\] = (\d+)$/)
      memory[address] = applyMask(mask, value);
    }
  }

  return Object.keys(memory).reduce((sum, address) =>
    sum + parseInt(memory[address])
    , 0)
}

function part2(input) {
  let lines = input
    .trim()
    .split('\n');

  let memory = {};
  let mask;
  for (let line of lines) {
    if (line.includes('mask')) {
      // Parse mask
      let [_, str] = line.split(' = ');
      mask = str.split("").map((b, i) => {
        return { i, b };
      });
    } else {
      // Parse memory
      let [_, address, value] = line.match(/^mem\[(\d+)\] = (\d+)$/)
      let target = applyMaskToMemory(mask, address);
      for (let m of target) {
        memory[m] = parseInt(value);
      }
    }
  }

  return Object.keys(memory).reduce((sum, address) =>
    sum + parseInt(memory[address])
    , 0)
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day14 part1:', part1(input));
log('day14 part2:', part2(input));

/*
const input = require('../input');
const log = console.log;
const compose = (...fns) => (args) => fns.reduceRight((arg, fn) => fn(arg), args);
const join = (char) => (array) => array.join(char);
const replace = (...args) => (string) => string.replace(...args);
const match = (pattern) => (string) => string.match(pattern) ?? [];
const map = (fn) => (array) => array.map(fn);

const parseBinaryStringToInt = (string) => parseInt(string, 2);
const replaceXWithOne = replace('X', '1');
const replaceXWithZero = replace('X', 0);
const toMaskAnd = compose(BigInt, parseBinaryStringToInt, join(''), map(replaceXWithOne));
const toMaskOr = compose(BigInt, parseBinaryStringToInt, join(''), map(replaceXWithZero));

const initialize = (instructions, masks = { '&': [], '|': [] }, memory = {}) => {
  const [instruction, ...remaining] = instructions;

  if (!instruction) return memory;

  if (instruction.mask) {
    masks['&'] = toMaskAnd([...instruction.mask]);
    masks['|'] = toMaskOr([...instruction.mask]);
  } else {
    memory[instruction.address] = (BigInt(instruction.value) & masks['&']) | masks['|'];
  }

  return initialize(remaining, masks, memory);
};

const matchMask = match(/^mask = ([01X]+)$/);
const toMask = ([_, mask]) => ({ mask });
const matchMemory = match(/^mem\[(\d+)] = (\d+)$/);
const toMemory = ([_, address, value]) => ({ address, value });
const parseLineToMaskOrMemory = (line) =>
  (line.startsWith('mask') ? compose(toMask, matchMask) : compose(toMemory, matchMemory))(line);

const instructions = input(__dirname, './input.txt').split('\n').map(parseLineToMaskOrMemory);

const memory = initialize(instructions);
log(`Solution pt.1 ${Object.values(memory).reduce((a, b) => a + b, 0n)}`);

log(`Solution pt.2 ${[]}`);
*/
