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

function printGrid(grid) {
  let points = Object.keys(grid).map(toCoords);
  let xs = points.map(p => p.x);
  let ys = points.map(p => p.y);
  let [minX, maxX] = [Math.min(...xs), Math.max(...xs)];
  let [minY, maxY] = [Math.min(...ys), Math.max(...ys)];
  for (let y = minY; y <= maxY; y++) {
    let line = '';
    for (let x = minX; x <= maxX; x++) {
      let value = grid[toComma({ x, y })] === undefined ? '.' : '#';
      line += value;
    }
    console.log(line);
  }
}

function move(coord, delta) {
  coord.x += delta.x;
  coord.y += delta.y;
}

function isNeighbor(coord1, coord2) {
  return Math.abs(coord1.x - coord2.x) <= 1 && Math.abs(coord1.y - coord2.y) <= 1;
}

function follow(self, target, visited) {
  if (!isNeighbor(self, target)) {
    let dx = Math.sign(target.x - self.x);
    let dy = Math.sign(target.y - self.y);
    self.x += dx;
    self.y += dy;
  }
}

function part1(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let visited = { '0,0': true };
  let head = { x: 0, y: 0 };
  let tail = { x: 0, y: 0 };

  for (let line of lines) {
    let [dir, steps] = line.split(' ');
    steps = parseInt(steps, 10);

    for (let i = 0; i < steps; i++) {
      move(head, DIRS[dir]);
      follow(tail, head, visited);
      visited[toComma(tail)] = true;
    }
  }
  // printGrid(visited);
  return Object.keys(visited).length;
}

function part2(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let visited = { '0,0': true };
  // let tails = new Array(10).fill().map(() => new Array(10).fill().map(() => ({ unique: true })));
  let size = 10;
  let knots = new Array(size).fill().map(() => ({ x: 0, y: 0 }));

  for (let line of lines) {
    let [dir, steps] = line.split(' ');
    steps = parseInt(steps, 10);

    for (let i = 0; i < steps; i++) {
      move(knots[0], DIRS[dir]);
      for (let j = 1; j < knots.length; j++) {
        follow(knots[j], knots[j - 1]);
      }
      visited[toComma(knots.at(-1))] = true;
    }
  }
  return Object.keys(visited).length;
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8').replaceAll('\r', '');

// input = `
// R 4
// U 4
// L 3
// D 1
// R 4
// D 1
// L 5
// R 2`;

// input = `
// R 5
// U 8
// L 8
// D 3
// R 17
// D 10
// L 25
// U 20`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
