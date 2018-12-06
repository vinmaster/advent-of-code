const fs = require('fs');
const path = require('path');

const part1 = (input) => {
  const lines = input
    .trim()
    .split('\n');
  let twice = 0;
  let thrice = 0;
  for (const line of lines) {
    let hash = {};
    let chars = line.split('');
    for (const char of chars) {
      hash[char] = hash[char] || 0;
      hash[char] += 1;
    }
    for (const key of Object.keys(hash)) {
      if (hash[key] === 2) {
        twice += 1;
        break;
      }
    }
    for (const key of Object.keys(hash)) {
      if (hash[key] === 3) {
        thrice += 1;
        break;
      }
    }
  }
  const checksum = twice * thrice;
  return checksum;
}

const part2 = (input) => {
  const lines = input
    .trim()
    .split('\n');
  let commonLetters = '';
  for (const line1 of lines) {
    if (commonLetters !== '') {
      break;
    }
    for (const line2 of lines) {
      let differ = 0;
      for (let i = 0; i < line1.length; i++) {
        if (line1.charAt(i) !== line2.charAt(i)) {
          differ += 1;
        } else {
          commonLetters += line1.charAt(i);
        }
      }
      if (differ !== 1) {
        commonLetters = '';
      } else {
        break;
      }
    }
  }
  return commonLetters;
}

const data = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day2 part1', part1(data));
console.log('day2 part2', part2(data));
