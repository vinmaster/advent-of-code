const fs = require('fs');
const path = require('path');

const part1 = input => {
  const measurements = input
    .trim()
    .split('\n')
    .map(s => parseInt(s, 10));

  let increased = 0;
  for (let index = 1; index < measurements.length; index++) {
    const cur = measurements[index];
    const prev = measurements[index - 1];
    if (cur > prev) increased++;
  }
  return increased;
};

const part2 = input => {
  let measurements = input
    .trim()
    .split('\n')
    .map(s => parseInt(s, 10));

  let increased = 0;
  for (let index = 3; index < measurements.length; index++) {
    let sum = (i) => measurements[i] + measurements[i - 1] + measurements[i - 2];
    const cur = sum(index);
    const prev = sum(index - 1);
    if (cur > prev) increased++;
  }
  return increased;
};

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day1 part1:', part1(input));
console.log('day1 part2:', part2(input));
