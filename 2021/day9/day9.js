const fs = require('fs');
const path = require('path');

let DIRS = {
  U: { y: -1, x: 0 },
  D: { y: 1, x: 0 },
  L: { y: 0, x: -1 },
  R: { y: 0, x: 1 },
};

function toComma(coord) {
  return `${coord.x},${coord.y}`;
}

function toCoords(str) {
  let [x, y] = str.split(',').map(s => parseInt(s, 10));
  return { x, y };
}

function isInside(grid, { x, y }) {
  return x >= 0 && y >= 0 && x < grid[0].length && y < grid.length;
}

function isNeighbor(coord1, coord2) {
  return Math.abs(coord1.x - coord2.x) <= 1 && Math.abs(coord1.y - coord2.y) <= 1;
}

function merge(coord1, coord2) {
  return { x: coord1.x + coord2.x, y: coord1.y + coord2.y };
}

function getNeighbors(grid, coord) {
  return Object.values(DIRS)
    .map(dir => merge(dir, coord))
    .filter(c => isInside(grid, c))
    .map(({ x, y }) => parseInt(grid[y][x], 10));
}

function basinSize(grid, coord, visited) {
  let { x, y } = coord;
  if (visited.includes(toComma(coord)) || !isInside(grid, coord) || grid[y][x] === '9') {
    return visited;
  }
  visited.push(toComma(coord));
  return Object.values(DIRS).flatMap(dir => basinSize(grid, merge(dir, coord), visited));
}

function part1(input) {
  /** @type string[] */
  let grid = input.trim().split('\n');

  let lowest = [];
  for (let y = 0; y < grid.length; y++) {
    for (let x = 0; x < grid[0].length; x++) {
      let neighbors = getNeighbors(grid, { x, y });
      let num = parseInt(grid[y][x], 10);
      if (neighbors.every(n => n > num)) {
        lowest.push(num);
      }
    }
  }

  return lowest.map(n => n + 1).reduce((a, b) => a + b);
}

function part2(input) {
  /** @type string[] */
  let grid = input.trim().split('\n');

  let lowest = [];
  for (let y = 0; y < grid.length; y++) {
    for (let x = 0; x < grid[0].length; x++) {
      let neighbors = getNeighbors(grid, { x, y });
      let num = parseInt(grid[y][x], 10);
      if (neighbors.every(n => n > num)) {
        lowest.push({ x, y });
      }
    }
  }

  let sizes = lowest.map(coord => [...new Set(basinSize(grid, coord, []))].length);
  sizes.sort((a, b) => b - a);

  return sizes.slice(0, 3).reduce((a, b) => a * b);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8').replaceAll('\r', '');

// input = `
// 2199943210
// 3987894921
// 9856789892
// 8767896789
// 9899965678`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
