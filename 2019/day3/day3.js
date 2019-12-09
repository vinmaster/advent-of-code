const fs = require('fs');
const path = require('path');

function traceWire(map, intersections, wire, type) {
  const wirePath = wire.split(',');
  let current = { x: 0, y: 0 };
  for (const p of wirePath) {
    const direction = p[0];
    const distance = parseInt(p.slice(1), 10);
    let [x, y] = [0, 0];

    if (direction === 'L') { x = -1; }
    else if (direction === 'R') { x = 1; }
    else if (direction === 'U') { y = 1; }
    else if (direction === 'D') { y = -1; }

    const destination = {
      x: current.x + (x * distance),
      y: current.y + (y * distance)
    };

    while (current.x !== destination.x || current.y !== destination.y) {
      current.x += x;
      current.y += y;

      const cell = map.get(`${current.x},${current.y}`);
      if (cell && cell !== type) {
        intersections.push(Object.assign({}, current));
      } else {
        map.set(`${current.x},${current.y}`, type);
      }
    }
  }
}

function traceWireWithDelay(map, intersections, wire, type) {
  const wirePath = wire.split(',');
  let current = { x: 0, y: 0 };
  let steps = 0;
  for (const p of wirePath) {
    const direction = p[0];
    const distance = parseInt(p.slice(1), 10);
    let [x, y] = [0, 0];

    if (direction === 'L') { x = -1; }
    else if (direction === 'R') { x = 1; }
    else if (direction === 'U') { y = 1; }
    else if (direction === 'D') { y = -1; }

    const destination = {
      x: current.x + (x * distance),
      y: current.y + (y * distance)
    };

    while (current.x !== destination.x || current.y !== destination.y) {
      current.x += x;
      current.y += y;
      steps += 1;

      const cell = map.get(`${current.x},${current.y}`);
      if (cell) {
        const [cellType, cellSteps] = cell;
        if (cellType !== type) {
          intersections.push(steps + cellSteps);
        }
      } else {
        map.set(`${current.x},${current.y}`, [type, steps]);
      }
    }
  }
}

function calculateManhattanDistance(p1, p2) {
  return Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y);
}

function part1(input) {
  const [wire1, wire2] = input
    .trim()
    .split('\n');

  const map = new Map();
  const intersections = [];

  traceWire(map, intersections, wire1, 'a');
  traceWire(map, intersections, wire2, 'b');

  return intersections.reduce((maxDistance, i) =>
    Math.min(maxDistance, calculateManhattanDistance({ x: 0, y: 0 }, i)), Number.MAX_SAFE_INTEGER
  );
}

function part2(input) {
  const [wire1, wire2] = input
    .trim()
    .split('\n');

  const map = new Map();
  const intersections = [];

  traceWireWithDelay(map, intersections, wire1, 'a');
  traceWireWithDelay(map, intersections, wire2, 'b');

  return intersections.reduce((minSteps, i) =>
    Math.min(minSteps, i), Number.MAX_SAFE_INTEGER
  );
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day3 part1:', part1(input));
console.log('day3 part2:', part2(input));

/*

const fs = require('fs');
const input = fs
    .readFileSync(__dirname + '/input/day-3', 'utf-8')
    .trim()
    .split("\n");

const start = new Date().getTime();

let paths = [];
let dx = {'L': -1, 'R': 1, 'D': 0, 'U':  0};
let dy = {'L':  0, 'R': 0, 'D': 1, 'U': -1};

input.forEach((wire, index) => {
    wire = wire.split(',');
    let x = 0, y = 0, s = 0;
    paths[index] = [];

    wire.forEach(move => {
        for (let i = 0, steps = parseInt(move.substr(1)); i < steps; i++) {
            x += dx[move[0]];
            y += dy[move[0]];
            paths[index][y + '.' + x] = ++s;
        }
    });
});

let intersections = Object.keys(paths[0]).filter({}.hasOwnProperty.bind(paths[1]));
let sums1 = [], sums2 = [];

intersections.forEach(coords => {
    [x, y] = coords.split('.');
    sums1.push(Math.abs(x) + Math.abs(y));
    sums2.push(paths[0][coords] + paths[1][coords]);
});

console.log('Part 1: ' + Math.min(...sums1));
console.log('Part 2: ' + Math.min(...sums2));

console.log('Finished in: ' + (new Date().getTime() - start) + 'ms');

*/