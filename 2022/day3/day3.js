const fs = require('fs');
const path = require('path');

function part1(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let priority = [];
  for (let i = 'a'.charCodeAt(0); i <= 'z'.charCodeAt(0); i++) {
    priority.push(String.fromCharCode(i));
  }
  for (let i = 'A'.charCodeAt(0); i <= 'Z'.charCodeAt(0); i++) {
    priority.push(String.fromCharCode(i));
  }

  let result = [];
  for (let line of lines) {
    let len = line.length;
    let comp1 = line.substring(0, len / 2).split('');
    let comp2 = line.substring(len / 2, len).split('');
    let shared = comp1.find(c => comp2.includes(c));

    result.push(shared);
  }

  return result.map(c => priority.indexOf(c) + 1).reduce((a, b) => a + b);
}

function part2(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let priority = [];
  for (let i = 'a'.charCodeAt(0); i <= 'z'.charCodeAt(0); i++) {
    priority.push(String.fromCharCode(i));
  }
  for (let i = 'A'.charCodeAt(0); i <= 'Z'.charCodeAt(0); i++) {
    priority.push(String.fromCharCode(i));
  }

  let result = [];
  for (let i = 0; i < lines.length; i += 3) {
    let group = lines.slice(i, i + 3).map(line => line.split(''));
    let shared = group[0].find(c => {
      return group[1].includes(c) && group[2].includes(c);
    });
    result.push(shared);
  }

  return result.map(c => priority.indexOf(c) + 1).reduce((a, b) => a + b);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8').replaceAll('\r', '');

// input = `
// vJrwpWtwJgWrhcsFMMfFFhFp
// jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
// PmmdzqPrVvPwwTWBwg
// wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
// ttgJtRGJQctTZtZT
// CrZsJsPPZsGzwwsLwLmpwMDw`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
