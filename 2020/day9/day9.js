const fs = require('fs');
const path = require('path');

function findInvalidNum(nums, preamble) {
  let list = [];
  let map = {};
  // Setup preamble
  for (let i = 0; i < preamble; i++) {
    list.push(nums[0]);
    map[nums[0].toString()] = true;
    nums.shift();
  }

  for (let n of nums) {
    if (list.length > preamble) {
      let remove = list.shift();
      delete map[remove.toString()];
    }
    // Check n is valid
    let isValid = (n) => {
      return list.filter(m => {
        let target = n - m;
        let found = map[target.toString()];
        return found && m !== found;
      }).length > 0;
    };
    if (!isValid(n)) {
      return n;
    }

    list.push(n);
    map[n.toString()] = true;
  }
}

function sum(list) { return list.reduce((a, b) => a + b, 0); }

function findContiguousList(nums, target) {
  let list = [];
  let i = 0;
  while (i < nums.length) {
    while (sum(list) < target && i < nums.length) {
      let n = nums[i];
      list.push(n);
      i++;
    }

    if (sum(list) === target) return list;

    while (sum(list) > target) {
      list.shift();
    }
  }
}

function part1(input) {
  let nums = input
    .trim()
    .split('\n')
    .map(n => parseInt(n, 10));

  return findInvalidNum(nums, 25);
}

function part2(input) {
  let nums = input
    .trim()
    .split('\n')
    .map(n => parseInt(n, 10));

  let target = findInvalidNum([...nums], 25);
  let list = findContiguousList([...nums], target);
  return Math.min(...list) + Math.max(...list);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

console.log('day9 part1:', part1(input));
console.log('day9 part2:', part2(input));
