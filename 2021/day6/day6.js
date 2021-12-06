const fs = require('fs');
const path = require('path');

function occurrences(str, target) {
  let count = 0;
  let pos = str.indexOf(target);

  while (pos > -1) {
    ++count;
    pos = str.indexOf(target, ++pos);
  }
  return count;
}

const part1 = input => {
  /** @type number[] */
  let nums = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  for (let day = 1; day <= 80; day++) {
    let addAmount = 0;
    for (let i = 0; i < nums.length; i++) {
      nums[i]--;
      if (nums[i] < 0) {
        nums[i] = 6;
        addAmount++;
      }
    }
    for (let i = 0; i < addAmount; i++) {
      nums.push(8);
    }
    // console.log('day', day, nums.join(','));
  }
  return nums.length;
};

const part2 = input => {
  /** @type number[] */
  let nums = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  let hash = {};
  for (let i = 0; i <= 8; i++) hash[i.toString()] = 0;
  for (let num of nums) {
    hash[num]++;
  }

  for (let day = 1; day <= 256; day++) {
    let zeroes = hash['0'];
    for (let i = 1; i <= 8; i++) {
      hash[(i - 1).toString()] = hash[i.toString()];
    }
    hash['6'] += zeroes;
    hash['8'] = zeroes;
  }
  return Object.values(hash).reduce((acc, cur) => acc + cur);
};

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

// input = `3,4,3,1,2`

console.log('day6 part1:', part1(input));
console.log('day6 part2:', part2(input));
