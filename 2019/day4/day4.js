const fs = require('fs');
const path = require('path');

function noDecreasing(n) {
  const s = n.toString();
  let lastChar = '';
  const sorted = s.split('');
  for (const c of sorted) {
    if (parseInt(lastChar, 10) > parseInt(c, 10)) { return false }
    lastChar = c;
  }
  return true;
}

function meetCriterias(n) {
  const haveDoubles = (n) => {
    const s = n.toString();
    let lastChar = '';
    const sorted = s.split('').sort();
    for (const c of sorted) {
      if (lastChar === c) { return true }
      lastChar = c;
    }
    return false;
  };

  return haveDoubles(n) && noDecreasing(n);
}

function meetCriterias2(n) {
  const haveDoubles = (n) => {
    const s = n.toString();
    let lastChar = '';
    let count = 0;
    const sorted = s.split('').sort();
    for (const c of sorted) {
      if (lastChar === c) { count += 1; }
      else {
        if (count === 2) { return true; }
        count = 1;
      }
      lastChar = c;
    }
    return count === 2;
  };

  return haveDoubles(n) && noDecreasing(n);
}

function part1(input) {
  const [num1, num2] = input
    .trim()
    .split('-')
    .map(s => parseInt(s, 10));

  const passwords = [];
  for (let n = num1; n <= num2; n++) {
    if (meetCriterias(n)) {
      passwords.push(n);
    }
  }
  return passwords.length;
}

function part2(input) {
  const [num1, num2] = input
    .trim()
    .split('-')
    .map(s => parseInt(s, 10));

  const passwords = [];
  for (let n = num1; n <= num2; n++) {
    if (meetCriterias2(n)) {
      passwords.push(n);
    }
  }
  return passwords.length;
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day4 part1:', part1(input));
console.log('day4 part2:', part2(input));

/*

const fs = require('fs');
const input = fs
                .readFileSync(__dirname + '/input/day-4', 'utf-8')
                .trim()
                .split('-');
const start = new Date().getTime();

let part1 = 0, part2 = 0;

for (let num = +input[0], max = +input[1]; num <= max; num++) {
    let sNum = num.toString();

    if (sNum !== sNum.split('').sort().join('')) {
        continue;
    }

    if (sNum.match(/(?:(?:(\d?)(?!\1))|^)(\d)\2(?!\2)/)) {
        part1++;
        part2++;
    } else if (sNum.match(/(\d)\1/)) {
        part1++;
    }
}

console.log('Part 1: ' + part1);
console.log('Part 2: ' + part2);

console.log('Finished in: ' + (new Date().getTime() - start) + 'ms');

*/
