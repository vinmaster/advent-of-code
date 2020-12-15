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

/*
const input = require('../input');
const log = console.log;
const count = (value) => (array) => array.reduce((total, v) => (v === value ? total + 1 : total), 0);

const Area = () => {
  const FLOOR = '.';
  const EMPTY = 'L';
  const OCCUPIED = '#';
  const countOccupiedSeats = count(OCCUPIED);

  const directions = [
    [-1, 0],
    [-1, 1],
    [0, 1],
    [1, 1],
    [1, 0],
    [1, -1],
    [0, -1],
    [-1, -1],
  ];

  const countOccupiedSeatsFromNeighbours = (layout, [selfX, selfY]) =>
    countOccupiedSeats(directions.map(([offsetX, offsetY]) => layout[selfY + offsetY]?.[selfX + offsetX]));

  const countOccupiedSeatsWithinLineOfSight = (layout, self, offsetBy = 1) =>
    countOccupiedSeats(directions.map((direction) => findOccupiedSeatForDirection(layout, self, direction, offsetBy)));

  const findOccupiedSeatForDirection = (layout, [selfX, selfY], [offsetX, offsetY], offsetBy) => {
    const neighbour = layout[selfY + offsetY * offsetBy]?.[selfX + offsetX * offsetBy];

    return neighbour && neighbour === FLOOR
      ? findOccupiedSeatForDirection(layout, [selfX, selfY], [offsetX, offsetY], ++offsetBy)
      : neighbour;
  };

  const hasReachedEquilibrium = (currentLayout, nextLayout) =>
    currentLayout.every((row, y) => row.every((column, x) => column === nextLayout[y][x]));

  const tick = (currentLayout, config) => {
    const nextLayout = currentLayout.reduce((nextLayout, row, selfY) => {
      nextLayout[selfY] = row.map((column, selfX) => {
        if (column === FLOOR) return FLOOR;

        const adjacentOccupiedSeatCount = config.isAdjacentLineOfSight
          ? countOccupiedSeatsWithinLineOfSight(currentLayout, [selfX, selfY])
          : countOccupiedSeatsFromNeighbours(currentLayout, [selfX, selfY]);

        if (column === EMPTY) {
          return adjacentOccupiedSeatCount === 0 ? OCCUPIED : EMPTY;
        }

        if (column === OCCUPIED) {
          return adjacentOccupiedSeatCount >= config.occupiedSeatThreshold ? EMPTY : OCCUPIED;
        }
      });
      return nextLayout;
    }, []);

    return hasReachedEquilibrium(currentLayout, nextLayout) ? nextLayout : tick(nextLayout, config);
  };

  return {
    run: tick,
  };
};

const area = Area();
const countOccupiedSeats = count('#');
const layout = input(__dirname, './input.txt')
  .split('\n')
  .map((row) => [...row]);

const solutionOne = countOccupiedSeats(
  area.run(layout, { occupiedSeatThreshold: 4, isAdjacentLineOfSight: false }).flat(),
);
log(`Solution pt.1 ${solutionOne}`);

const solutionTwo = countOccupiedSeats(
  area.run(layout, { occupiedSeatThreshold: 5, isAdjacentLineOfSight: true }).flat(),
);
log(`Solution pt.2 ${solutionTwo}`);
*/