const fs = require('fs');
const path = require('path');

const part1 = input => {
  const expenses = input
    .trim()
    .split('\n')
    .map(s => parseInt(s, 10));

  const target = 2020;
  for (const num1 of expenses) {
    for (const num2 of expenses) {
      if (num1 + num2 === target) {
        return num1 * num2;
      }
    }
  }
};

const part2 = input => {
  const expenses = input
    .trim()
    .split('\n')
    .map(s => parseInt(s, 10));

  const target = 2020;
  for (const num1 of expenses) {
    for (const num2 of expenses) {
      for (const num3 of expenses) {
        if (num1 + num2 + num3 === target) {
          return num1 * num2 * num3;
        }
      }
    }
  }
};

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day1 part1:', part1(input));
console.log('day1 part2:', part2(input));
