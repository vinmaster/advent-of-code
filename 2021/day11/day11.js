const fs = require('fs');
const path = require('path');

// coord is 'x,y' as string
// point is array with x and y as number

function toCoord([x, y]) {
  return `${x},${y}`;
}

/**
 * @param {string} coord
 */
function toPoints(coord) {
  return coord.split(',').map(s => parseInt(s, 10));
}

function printGrid(grid) {
  let points = Object.keys(grid).map(toPoints);
  let xs = points.map(p => p[0]);
  let ys = points.map(p => p[1]);
  let [minX, maxX] = [Math.min(...xs), Math.max(...xs)];
  let [minY, maxY] = [Math.min(...ys), Math.max(...ys)];
  for (let y = minY; y <= maxY; y++) {
    let line = '';
    for (let x = minX; x <= maxX; x++) {
      let value = grid[toCoord([x, y])] === undefined ? '.' : grid[toCoord([x, y])];
      line += value;
    }
    console.log(line);
  }
}

function getAdjacentCoords(coord) {
  let [x, y] = toPoints(coord);
  let d = [
    [-1, -1],
    [0, -1],
    [1, -1],
    [-1, 0],
    [1, 0],
    [-1, 1],
    [0, 1],
    [1, 1],
  ];
  return d.map(([dx, dy]) => toCoord([x + dx, y + dy]));
}

function inGrid(grid, coord) {
  let [x, y] = toPoints(coord);
  // let maxX = 10;
  // let maxY = 10;
  // return x >= 0 && y >= 0 && y < maxY && x < maxX;
  return x >= 0 && y >= 0 && y < grid.length && x < grid[0].length;
}

const part1 = input => {
  /** @type string[] */
  let lines = input.trim().split('\n');

  let grid = {};
  for (let y = 0; y < lines.length; y++) {
    let line = lines[y];
    for (let x = 0; x < line.length; x++) {
      grid[`${x},${y}`] = parseInt(line[x], 10);
    }
  }

  let flashes = 0;
  for (let step = 1; step <= 100; step++) {
    let queue = Object.keys(grid);

    while (queue.length !== 0) {
      let coord = queue.shift();
      let [x, y] = toPoints(coord);
      if (grid[coord] !== ' ') grid[coord]++;
      if (grid[coord] > 9) {
        flashes++;
        grid[coord] = ' ';
        let coords = getAdjacentCoords(coord).filter(c => inGrid(lines, c));
        queue.push(...coords);
      }
    }

    for (let [key, value] of Object.entries(grid)) {
      if (value === ' ') {
        grid[key] = 0;
      }
    }
    // printGrid(grid);
    // console.log();
  }
  return flashes;
};

const part2 = input => {
  /** @type string[] */
  let lines = input.trim().split('\n');

  let grid = {};
  for (let y = 0; y < lines.length; y++) {
    let line = lines[y];
    for (let x = 0; x < line.length; x++) {
      grid[`${x},${y}`] = parseInt(line[x], 10);
    }
  }

  let flashes = 0;
  for (let step = 1; step <= 300; step++) {
    let queue = Object.keys(grid);

    while (queue.length !== 0) {
      let coord = queue.shift();
      let [x, y] = toPoints(coord);
      if (grid[coord] !== ' ') grid[coord]++;
      if (grid[coord] > 9) {
        flashes++;
        grid[coord] = ' ';
        let coords = getAdjacentCoords(coord).filter(c => inGrid(lines, c));
        queue.push(...coords);
      }
    }

    if (Object.keys(grid).every(coord => grid[coord] === ' ')) {
      return step;
    }

    for (let [key, value] of Object.entries(grid)) {
      if (value === ' ') {
        grid[key] = 0;
      }
    }
    // printGrid(grid);
    // console.log();
  }
};

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

// input = `
// 5483143223
// 2745854711
// 5264556173
// 6141336146
// 6357385478
// 4167524645
// 2176841721
// 6882881134
// 4846848554
// 5283751526`;

console.log('day11 part1:', part1(input));
console.log('day11 part2:', part2(input));
