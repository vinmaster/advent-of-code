const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
util.inspect.defaultOptions.showHidden = true;
util.inspect.defaultOptions.depth = null;
util.inspect.defaultOptions.compact = true;

const log = (...args) => console.log(...args);

function part1(input) {
  let lines = input
    .trim()
    .split('\n');
  let regex = /^(\w)(\d+)$/;
  let DIRS = [
    { name: 'N', dv: [0, 1] },
    { name: 'E', dv: [1, 0] },
    { name: 'S', dv: [0, -1] },
    { name: 'W', dv: [-1, 0] },
  ];
  let [x, y] = [0, 0];
  let facing = 'E';


  for (let line of lines) {
    let [_, action, value] = line.match(regex) || [];
    let [dx, dy] = [null, null];
    let move = false;
    let i = DIRS.map(d => d.name).indexOf(facing);

    if (['N', 'E', 'S', 'W'].includes(action)) {
      let dir = DIRS.find(d => d.name === action);
      ([dx, dy] = [dir.dv[0], dir.dv[1]]);
      move = true;
    } else if (action === 'F') {
      let dir = DIRS.find(d => d.name === facing);
      ([dx, dy] = [dir.dv[0], dir.dv[1]]);
      move = true;
    } else if (action === 'L') {
      value /= 90;
      value *= -1;
      value += i;
      let index = (value + 4) % 4
      let dir = DIRS[index];
      facing = dir.name;
    } else if (action === 'R') {
      value /= 90;
      value += i;
      let index = (value + 4) % 4;
      let dir = DIRS[index];
      facing = dir.name;
    }
    if (move) {
      dx *= value;
      dy *= value;
      x += dx;
      y += dy;
    }
    // log(line, facing, x, y);
  }
  return Math.abs(x) + Math.abs(y);
}

function part2(input) {
  let lines = input
    .trim()
    .split('\n');
  let regex = /^(\w)(\d+)$/;
  let DIRS = [
    { name: 'N', dv: [0, 1] },
    { name: 'E', dv: [1, 0] },
    { name: 'S', dv: [0, -1] },
    { name: 'W', dv: [-1, 0] },
  ];
  let [x, y] = [0, 0];
  // Waypoint
  let [wx, wy] = [10, 1];
  let facing = 'E';

  for (let line of lines) {
    let [_, action, value] = line.match(regex) || [];
    let [dx, dy] = [null, null];
    let move = false;
    let i = DIRS.map(d => d.name).indexOf(facing);

    if (['N', 'E', 'S', 'W'].includes(action)) {
      let dir = DIRS.find(d => d.name === action);
      ([dx, dy] = [dir.dv[0], dir.dv[1]]);
      dx *= value;
      dy *= value;
      wx += dx;
      wy += dy;
    } else if (action === 'F') {
      ([dx, dy] = [wx * value, wy * value]);
      move = true;
    } else if (action === 'L') {
      value /= 90;
      for (let i = 0; i < value; i++) {
        ([wx, wy] = [-wy, wx]);
      }
    } else if (action === 'R') {
      value /= 90;
      for (let i = 0; i < value; i++) {
        ([wx, wy] = [wy, -wx]);
      }
    }
    if (move) {
      x += dx;
      y += dy;
    }
    // log(line, facing, x, y);
  }
  return Math.abs(x) + Math.abs(y);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day12 part1:', part1(input));
log('day12 part2:', part2(input));

/*
const input = require('../input');
const log = console.log;
const match = (pattern) => (string) => string.match(pattern) ?? [];
const mod = (n, mod) => ((n % mod) + mod) % mod;
const manhattanDistance = (x, y) => Math.abs(x) + Math.abs(y);

// Split into two parts, as part two has no concept of direction but instead uses a waypoint. I could've reused the waypoint as the direction for
// part one but that would've been confusing. This however leads to some duplication between the solutions.

const DIRECTIONS = ['N', 'E', 'S', 'W'];
const DELTAS = {
  N: { x: 0, y: 1 },
  E: { x: 1, y: 0 },
  S: { x: 0, y: -1 },
  W: { x: -1, y: 0 },
};

const move = (position, amount, direction) => ({
  x: position.x + direction.x * amount,
  y: position.y + direction.y * amount,
});
const rotate = (direction, amount) => mod(DIRECTIONS.indexOf(direction) + amount / 90, 4);

const reducer = (ship, { action, amount }) => {
  switch (action) {
    case 'L':
      return { ...ship, direction: DIRECTIONS[rotate(ship.direction, -amount)] };
    case 'R':
      return { ...ship, direction: DIRECTIONS[rotate(ship.direction, amount)] };
    case 'F':
      return { ...ship, position: move(ship.position, amount, DELTAS[ship.direction]) };
    default:
      return { ...ship, position: move(ship.position, amount, DELTAS[action]) };
  }
};

const run = (instructions, ship) => instructions.reduce((ship, instruction) => reducer(ship, instruction), ship);

const parseInstruction = ([_, action, amount]) => ({ action, amount });
const instructions = input(__dirname, './input.txt')
  .split('\n')
  .map(match(/^([NSEWLRF])(\d+)$/))
  .map(parseInstruction);

const ship = run(instructions, { position: { x: 0, y: 0 }, direction: 'E' });
const solution = manhattanDistance(ship.position.x, ship.position.y);
log(`Solution pt.1 ${solution}`);



const input = require('../input');
const log = console.log;
const match = (pattern) => (string) => string.match(pattern) ?? [];
const mod = (n, mod) => ((n % mod) + mod) % mod;
const manhattanDistance = (x, y) => Math.abs(x) + Math.abs(y);

const DELTAS = {
  N: { x: 0, y: 1 },
  E: { x: 1, y: 0 },
  S: { x: 0, y: -1 },
  W: { x: -1, y: 0 },
};

const move = (position, amount, direction) => ({
  x: position.x + direction.x * amount,
  y: position.y + direction.y * amount,
});
const rotate = (position, times) => Array.from({ length: times }).reduce(({ x, y }) => ({ x: -y, y: x }), position);

const reducer = (ship, { action, amount }) => {
  switch (action) {
    case 'L':
      return { ...ship, waypoint: rotate(ship.waypoint, mod(amount / 90, 4)) };
    case 'R':
      return { ...ship, waypoint: rotate(ship.waypoint, 4 - mod(amount / 90, 4)) };
    case 'F':
      return { ...ship, position: move(ship.position, amount, ship.waypoint) };
    default:
      return { ...ship, waypoint: move(ship.waypoint, amount, DELTAS[action]) };
  }
};

const run = (instructions, ship) => instructions.reduce((ship, instruction) => reducer(ship, instruction), ship);

const parseInstruction = ([_, action, amount]) => ({ action, amount });
const instructions = input(__dirname, './input.txt')
  .split('\n')
  .map(match(/^([NSEWLRF])(\d+)$/))
  .map(parseInstruction);

const ship = run(instructions, { position: { x: 0, y: 0 }, waypoint: { x: 10, y: 1 } });
const solution = manhattanDistance(ship.position.x, ship.position.y);
log(`Solution pt.2 ${solution}`);
*/