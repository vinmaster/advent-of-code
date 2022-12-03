const fs = require('fs');
const path = require('path');

function part1(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let youMap = ['A', 'B', 'C'];
  let meMap = ['X', 'Y', 'Z'];

  let score = 0;
  for (let line of lines) {
    let [you, me] = line.split(' ');
    let youIndex = youMap.indexOf(you);
    let meIndex = meMap.indexOf(me);
    let result = 0;
    if (meIndex === youIndex) {
      result = 3;
    } else if (youIndex === 0 && meIndex === 2) {
      result = 0;
    } else if (meIndex > youIndex || (youIndex === 2 && meIndex === 0)) {
      result = 6;
    }
    score += meIndex + 1 + result;
  }
  return score;
}

function part2(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let youMap = ['A', 'B', 'C'];
  let meMap = ['X', 'Y', 'Z'];

  let score = 0;
  for (let line of lines) {
    let [you, me] = line.split(' ');
    let youIndex = youMap.indexOf(you);
    let meIndex;
    if (me === 'Y') {
      // draw
      meIndex = youIndex;
    } else if (me === 'X') {
      // lose
      meIndex = (youIndex - 1 + 3) % 3;
    } else {
      // win
      meIndex = (youIndex + 1 + 3) % 3;
    }
    let result = 0;
    if (meIndex === youIndex) {
      result = 3;
    } else if (youIndex === 0 && meIndex === 2) {
      result = 0;
    } else if (meIndex > youIndex || (youIndex === 2 && meIndex === 0)) {
      result = 6;
    }
    score += meIndex + 1 + result;
  }
  return score;
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8').replaceAll('\r', '');

// input = `
// A Y
// B X
// C Z`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
