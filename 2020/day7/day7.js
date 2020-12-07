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
