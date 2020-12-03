const fs = require('fs');
const path = require('path');

const part1 = input => {
  const lines = input
    .trim()
    .split('\n');

  const dx = 3;
  const dy = 1;
  let trees = 0;
  let x = dx;
  for (let y = dy; y < lines.length; y += dy) {
    if (x >= lines[y].length) x = x % lines[y].length;
    const hasTree = lines[y][x] === '#';
    if (hasTree) trees += 1;

    x += dx;
  }

  return trees;
};

const part2 = input => {
  const lines = input
    .trim()
    .split('\n');

  const slopes = [
    { dx: 1, dy: 1 },
    { dx: 3, dy: 1 },
    { dx: 5, dy: 1 },
    { dx: 7, dy: 1 },
    { dx: 1, dy: 2 },
  ]
  const treesPerSlope = []
  for (const { dx, dy } of slopes) {
    let trees = 0;
    let x = dx;
    for (let y = dy; y < lines.length; y += dy) {
      if (x >= lines[y].length) x = x % lines[y].length;
      const hasTree = lines[y][x] === '#';
      if (hasTree) trees += 1;

      x += dx;
    }
    treesPerSlope.push(trees);
  }

  return treesPerSlope.reduce((curr, acc) => {
    acc *= curr;
    return acc;
  });
};

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day3 part1:', part1(input));
console.log('day3 part2:', part2(input));
