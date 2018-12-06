const fs = require('fs');
const path = require('path');

const part1 = (input) => {
  const lines = input
    .trim()
    .split('\n')
    .map(s => parseInt(s, 10));
  let total = 0;
  for (const line of lines) {
    total += line;
  }
  return total;
}

const part2 = (input) => {
  const lines = input.trim().split('\n');
  let total = 0;
  let hash = {};
  let found = false;
  while (!found) {
    for (const line of lines) {
      total += parseInt(line, 10);
      if (hash[total] === undefined) {
        hash[total] = 1;
      } else {
        hash[total] += 1;
        found = true;
        break;
      }
    }
  }
  return total;
}

const data = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day1 part1:', part1(data));
console.log('day1 part2:', part2(data));
