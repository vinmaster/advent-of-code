const fs = require('fs');
const path = require('path');

const occurrences = (arr) => arr.reduce(function (acc, curr) {
  return acc[curr] ? ++acc[curr] : acc[curr] = 1, acc
}, {});

const part1 = input => {
  /** @type string[] */
  let lines = input
    .trim()
    .split('\n');

  let results = [];
  for (let line of lines) {
    for (let i = 0; i < line.length; i++) {
      if (results.length < i + 1) results.push([]);
      results[i].push(line[i]);
    }
  }

  let gamma = ''; // most common
  let epsilon = ''; // least common
  for (let bag of results) {
    let counts = occurrences(bag);
    if (counts['0'] > counts['1']) {
      gamma += '0';
      epsilon += '1';
    } else {
      gamma += '1';
      epsilon += '0';
    }
  }

  return parseInt(gamma, 2) * parseInt(epsilon, 2);
};

const part2 = input => {
  /** @type string[] */
  let lines = input
    .trim()
    .split('\n');

  let oxygenGeneratorRating = ''; // most common
  let co2Scrubber = ''; // least common
  let length = lines[0].length;

  let lines1 = [...lines];
  for (let index = 0; index < length; index++) {
    let zeroes = 0;
    let ones = 0;
    for (let line of lines1) {
      if (line[index] === '0') zeroes++;
      else ones++;
    }
    if (zeroes > ones) oxygenGeneratorRating += '0';
    else oxygenGeneratorRating += '1';

    for (let i = 0; i < lines1.length;) {
      if (lines1[i][index] !== oxygenGeneratorRating[oxygenGeneratorRating.length - 1]) {
        lines1.splice(i, 1);
      } else {
        i++;
      }
      if (lines1.length === 1) {
        oxygenGeneratorRating = lines1[0];
        break;
      }
    }
    if (lines1.length === 1) break;
  }

  let lines2 = [...lines];
  for (let index = 0; index < length; index++) {
    let zeroes = 0;
    let ones = 0;
    for (let line of lines2) {
      if (line[index] === '0') zeroes++;
      else ones++;
    }
    if (zeroes <= ones) co2Scrubber += '0';
    else co2Scrubber += '1';

    for (let i = 0; i < lines2.length;) {
      if (lines2[i][index] !== co2Scrubber[co2Scrubber.length - 1]) {
        lines2.splice(i, 1);
      } else {
        i++;
      }
      if (lines2.length === 1) {
        co2Scrubber = lines2[0];
        break;
      }
    }
    if (lines2.length === 1) break;
  }

  return parseInt(oxygenGeneratorRating, 2) * parseInt(co2Scrubber, 2);
};

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

// input = `00100
// 11110
// 10110
// 10111
// 10101
// 01111
// 00111
// 11100
// 10000
// 11001
// 00010
// 01010`

console.log('day3 part1:', part1(input));
console.log('day3 part2:', part2(input));
