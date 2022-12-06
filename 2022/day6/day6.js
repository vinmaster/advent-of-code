const fs = require('fs');
const path = require('path');

function part1(input) {
  /** @type string */
  let line = input.trim();
  let markerSize = 4;
  for (let i = 0; i < line.length - markerSize; i++) {
    if (new Set(line.substring(i, i + markerSize).split('')).size === markerSize) {
      return i + markerSize;
    }
  }
}

function part2(input) {
  /** @type string */
  let line = input.trim();
  let markerSize = 14;
  for (let i = 0; i < line.length - markerSize; i++) {
    if (new Set(line.substring(i, i + markerSize).split('')).size === markerSize) {
      return i + markerSize;
    }
  }
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8').replaceAll('\r', '');

// input = `mjqjpqmgbljsphdztnvjfqwrcgsmlb`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
