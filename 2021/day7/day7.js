const fs = require('fs');
const path = require('path');

function range(...args) {
  // end only
  if (args.length === 1) return [...Array(args[0]).keys()];
  // start + end
  else if (args.length === 2) return [...Array(args[1] - args[0]).keys()].map(i => i + args[0]);
}

function sum(arr) {
  if (arr.length === 0) return 0;
  return arr.reduce((acc, cur) => acc + cur);
}

// sum of natural numbers
function rangeSum(n) {
  return (n * (n + 1)) / 2;
}

const part1 = input => {
  /** @type number[] */
  let nums = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  let [min, max] = [Math.min(...nums), Math.max(...nums)];
  let sumFuels = [];
  for (let x = min; x <= max; x++) {
    let sum = 0;
    for (let num of nums) {
      sum += Math.abs(x - num);
    }
    sumFuels.push(sum);
  }
  return Math.min(...sumFuels);
};

const part2 = input => {
  /** @type number[] */
  let nums = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  let [min, max] = [Math.min(...nums), Math.max(...nums)];
  let sumFuels = [];
  for (let x = min; x <= max; x++) {
    let currentSum = 0;
    for (let num of nums) {
      // currentSum += sum(range(1, Math.abs(x - num) + 1));
      currentSum += rangeSum(Math.abs(x - num));
    }
    sumFuels.push(currentSum);
  }
  return Math.min(...sumFuels);
};

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

// input = `16,1,2,0,4,2,7,1,2,14`

console.log('day7 part1:', part1(input));
console.log('day7 part2:', part2(input));
