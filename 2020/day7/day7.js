const fs = require('fs');
const path = require('path');

function reverseMapping(mapping) {
  let current = {};
  let queue = Object.keys(mapping);
  while (queue.length !== 0) {
    let key = queue.shift();
    let list = mapping[key];

    // If current key exists in mapping, check for references
    if (current[key]) {
      for (let [key2, values] of Object.entries(current)) {
        if (values.includes(key)) {
          current[key2] = [...new Set(current[key2].concat(current[key]))];
        }
      }
    }

    for (let element of list) {
      if (!current[element]) current[element] = [];
      if (!current[element].includes(key)) {
        current[element].push(key);
        queue.push(key);
      }
    }
  }
  return current;
}

function addBags(mapping, name) {
  let count = 1;
  for (let bag of mapping[name]) {
    count += bag.num * addBags(mapping, bag.color);
  }
  return count;
}

function part1(input) {
  const lines = input
    .trim()
    .split('\n');

  let bagReg = /(\d+) (\w+ \w+) bag[s]?/;
  let mapping = {};

  for (let line of lines) {
    let [, outer, inner] = line.match(/^(\w+[ ]?\w+) bags contain (no other bags.|([, ]?\d+ \w+[ ]?\w+ bag[s]?[, ]?){0,}.)/) ?? [];

    let innerBags = inner.split(/, /).map(s => {
      let [_, num, color] = s.match(bagReg) ?? [];
      return color;
    });
    if (!mapping[outer]) {
      mapping[outer] = [];
    }
    innerBags = innerBags.filter(b => !!b);
    innerBags.map(b => mapping[outer].push(b));
  }
  let reverse = reverseMapping(mapping);

  return reverse['shiny gold'].length;
}

function part2(input) {
  const lines = input
    .trim()
    .split('\n');

  let bagReg = /(\d+) (\w+ \w+) bag[s]?/;
  let mapping = {};

  for (let line of lines) {
    let [, outer, inner] = line.match(/^(\w+[ ]?\w+) bags contain (no other bags.|([, ]?\d+ \w+[ ]?\w+ bag[s]?[, ]?){0,}.)/) ?? [];

    let innerBags = inner.split(/, /).map(s => {
      let [_, num, color] = s.match(bagReg) ?? [];
      return { num, color };
    });
    if (!mapping[outer]) {
      mapping[outer] = [];
    }
    innerBags = innerBags.filter(b => !!b.color);
    innerBags.map(b => mapping[outer].push(b));
  }

  return addBags(mapping, 'shiny gold') - 1;
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day7 part1:', part1(input));
console.log('day7 part2:', part2(input));


// const input = require('../input');
// const log = console.log;
// const compose = (...fns) => (args) => fns.reduceRight((arg, fn) => fn(arg), args);
// const match = (pattern) => (string) => string.match(pattern) ?? [];

// const toBag = ([_, type, contents]) => ({
//   type,
//   contents: contents.split(',').map(toBagContentFromRule).filter(hasPositiveCount),
// });
// const toBagContent = ([_, count, type]) => ({ count: +count, type });
// const hasPositiveCount = ({ count }) => count;
// const toBagFromRule = compose(toBag, match(/^(\w+ \w+) bags contain (.*)\.$/));
// const toBagContentFromRule = compose(toBagContent, match(/^\s*(\d+) (\w+ \w+).*/));

// const hasBagInContents = (bags, bag) => (contents = []) => {
//   return contents.some(({ type }) => type === bag)
//     ? true
//     : contents.some(({ type }) => hasBagInContents(bags, bag)(bags[type]));
// };

// const countBagsInContents = (bags, contents = []) =>
//   contents.reduce((total, { count, type }) => total + count + count * countBagsInContents(bags, bags[type]), 0);

// const bags = input(__dirname, './input.txt')
//   .split('\n')
//   .map(toBagFromRule)
//   .reduce((bags, { type, contents }) => ({ ...bags, [type]: contents }), {});

// const hasShinyGoldBagInContents = hasBagInContents(bags, 'shiny gold');
// const solutionOne = Object.values(bags).filter(hasShinyGoldBagInContents).length;
// log(`Solution pt.1 ${solutionOne}`);

// const solutionTwo = countBagsInContents(bags, bags['shiny gold']);
// log(`Solution pt.2 ${solutionTwo}`);
