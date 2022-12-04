const fs = require('fs');
const path = require('path');

function part1(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');

  let overlap = 0;
  for (let line of lines) {
    let [a1, a2] = line.split(',');
    let s1 = a1.split('-').map(x => parseInt(x, 10));
    let s2 = a2.split('-').map(x => parseInt(x, 10));
    // s1 bigger than s2
    if (s1[0] <= s2[0] && s1[1] >= s2[1]) overlap++;
    // s1 bigger than s2
    else if (s2[0] <= s1[0] && s2[1] >= s1[1]) overlap++;
  }
  return overlap;
}

function part2(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');

  let overlap = 0;
  for (let line of lines) {
    let [a1, a2] = line.split(',');
    let s1 = a1.split('-').map(x => parseInt(x, 10));
    let s2 = a2.split('-').map(x => parseInt(x, 10));
    let len1 = s1[1] - s1[0];
    let len2 = s2[1] - s2[0];

    // make sure s1 is smaller
    if (s1[0] > s2[0]) {
      [s1, s2] = [s2, s1];
      [len1, len2] = [len2, len1];
    }

    if (s1[0] + len1 >= s2[0]) overlap++;
  }
  return overlap;
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8').replaceAll('\r', '');

// input = `
// 2-4,6-8
// 2-3,4-5
// 5-7,7-9
// 2-8,3-7
// 6-6,4-6
// 2-6,4-8`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
