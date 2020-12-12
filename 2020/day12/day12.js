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
