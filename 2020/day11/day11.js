const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
util.inspect.defaultOptions.showHidden = true;
util.inspect.defaultOptions.depth = null;
util.inspect.defaultOptions.compact = true;

const log = (...args) => console.log(...args);
const isBetween = (low, high, val) => val >= low && val <= high;
const FLOOR = '.';
const EMPTY = 'L';
const OCCUPIED = '#';

function toTile(lines) {
  let tile = {}
  for (let y = 0; y < lines.length; y++) {
    for (let x = 0; x < lines[y].length; x++) {
      tile[`${x},${y}`] = lines[y][x];
    }
  }
  return tile;
}

function runRound(tile, tolerance = 4, lookInDirection = false) {
  // Clockwise from top left
  let dir = [[-1, -1], [-1, 0], [-1, 1],
  [0, -1], [0, 1],
  [1, -1], [1, 0], [1, 1]];
  let nextTile = {};
  for (let coord of Object.keys(tile)) {
    let [x, y] = coord.split(',').map(Number);
    let current = tile[`${x},${y}`];

    let occupiedCount;
    if (lookInDirection) {
      occupiedCount = dir.map(([dx, dy]) => {
        // Look in direction
        let a = x;
        let b = y;
        let seat;
        do {
          a += dx;
          b += dy;
          seat = tile[`${a},${b}`];
        } while (seat === FLOOR && seat !== undefined)
        return [a, b];
      }).filter(([x, y]) => tile[`${x},${y}`] === OCCUPIED).length;
    } else {
      occupiedCount = dir.map(([dx, dy]) => [x + dx, y + dy]).filter(([x, y]) => tile[`${x},${y}`] === OCCUPIED).length;
    }

    if (current === EMPTY && occupiedCount === 0) {
      nextTile[coord] = OCCUPIED;
    } else if (current === OCCUPIED && occupiedCount >= tolerance) {
      nextTile[coord] = EMPTY;
    } else {
      nextTile[coord] = current;
    }
  }
  return nextTile;
}

function toString(tile) {
  let s = '';
  let coords = Object.keys(tile);
  let maxX = 0;
  let maxY = 0;
  for (let coord of coords) {
    let [x, y] = coord.split(',').map(Number);
    if (x > maxX) maxX = x;
    if (y > maxY) maxY = y;
  }
  for (let y = 0; y <= maxY; y++) {
    for (let x = 0; x <= maxX; x++) {
      s += tile[`${x},${y}`];
    }
    s += '\n';
  }
  return s;
}

function countType(grid, type) {
  return Object.values(grid).filter(t => t === type).length;
}

function part1(input) {
  let lines = input
    .trim()
    .split('\n');
  let tile = toTile(lines);
  let lastS = null;
  let s = toString(tile);

  while (lastS !== s) {
    tile = runRound(tile);
    lastS = s;
    s = toString(tile);
  }
  // log(s);
  return countType(tile, OCCUPIED);
}

function part2(input) {
  let lines = input
    .trim()
    .split('\n');
  let tile = toTile(lines);
  let lastS = null;
  let s = toString(tile);

  while (lastS !== s) {
    tile = runRound(tile, 5, true);
    lastS = s;
    s = toString(tile);
  }
  // log(s);
  return countType(tile, OCCUPIED);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day11 part1:', part1(input));
log('day11 part2:', part2(input));
