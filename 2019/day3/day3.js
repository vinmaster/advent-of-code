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
