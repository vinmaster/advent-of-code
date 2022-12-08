const fs = require('fs');
const path = require('path');

let DIRS = [
  { y: -1, x: 0 },
  { y: 1, x: 0 },
  { y: 0, x: -1 },
  { y: 0, x: 1 },
];

function isInside(grid, x, y) {
  return x >= 0 && y >= 0 && x < grid[0].length && y < grid.length;
}

function collectDirsTrees(grid, x, y) {
  let positions = [[], [], [], []];
  for (let i = 0; i < DIRS.length; i++) {
    let dir = DIRS[i];
    let [x2, y2] = [x, y];
    while (true) {
      x2 += dir.x;
      y2 += dir.y;
      if (isInside(grid, x2, y2)) positions[i].push(grid[y2][x2]);
      else break;
    }
  }
  return positions;
}

function part1(input) {
  /** @type string[] */
  let grid = input.trim().split('\n');
  let visible = 0;
  let decreasingFrom = from => list => list.every(num => num < from);

  for (let y = 0; y < grid.length; y++) {
    let row = grid[y];
    for (let x = 0; x < row.length; x++) {
      let dirsTrees = collectDirsTrees(grid, x, y);
      let isVisible = dirsTrees.some(decreasingFrom(grid[y][x]));
      if (isVisible) {
        visible++;
      }
    }
  }
  return visible;
}

function part2(input) {
  /** @type string[] */
  let grid = input.trim().split('\n');
  let allScores = [];
  let distance = from => list => {
    let index = list.findIndex(tree => tree >= from);
    return index === -1 ? list.length : index + 1;
  };

  for (let y = 0; y < grid.length; y++) {
    let row = grid[y];
    for (let x = 0; x < row.length; x++) {
      let dirsTrees = collectDirsTrees(grid, x, y);
      let scenicScore = dirsTrees.map(distance(grid[y][x])).reduce((a, b) => a * b);
      allScores.push(scenicScore);
    }
  }
  return Math.max(...allScores);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8').replaceAll('\r', '');

// input = `
// 30373
// 25512
// 65332
// 33549
// 35390`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
