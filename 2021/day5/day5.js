const fs = require('fs');
const path = require('path');

// coord is 'x,y' as string
// point is array with x and y as number

function toCoord([x, y]) {
  return `${x},${y}`;
}

function toPoints(coord) {
  return coord.split(',').map(s => parseInt(s, 10));
}

// only get horizontal and vertical points
function getLinePoints(x1, y1, x2, y2) {
  let points = [];
  if (x1 === x2) {
    if (y1 < y2) for (let i = y1; i <= y2; i++) points.push([x1, i]);
    else for (let i = y1; i >= y2; i--) points.push([x1, i]);
  }
  if (y1 === y2) {
    if (x1 < x2) for (let i = x1; i <= x2; i++) points.push([i, y1]);
    else for (let i = x1; i >= x2; i--) points.push([i, y1]);
  }
  return points;
}

function getLinePointsWithDiagonal(x1, y1, x2, y2) {
  let points = [];
  if (x1 === x2) {
    if (y1 < y2) for (let i = y1; i <= y2; i++) points.push([x1, i]);
    else for (let i = y1; i >= y2; i--) points.push([x1, i]);
  } if (y1 === y2) {
    if (x1 < x2) for (let i = x1; i <= x2; i++) points.push([i, y1]);
    else for (let i = x1; i >= x2; i--) points.push([i, y1]);
  } else if (Math.abs(x1 - x2) === Math.abs(y1 - y2)) {
    // top-left to bottom-right
    if (x1 < x2 && y1 < y2) for (let i = 0; i <= x2 - x1; i++) points.push([x1 + i, y1 + i]);
    // top-right to bottom-left
    else if (x1 > x2 && y1 < y2) for (let i = 0; i <= x1 - x2; i++) points.push([x1 - i, y1 + i]);
    // bottom-left to top-right
    else if (x1 < x2 && y1 > y2) for (let i = 0; i <= x2 - x1; i++) points.push([x1 + i, y1 - i]);
    // bottom-right to top-left
    else if (x1 > x2 && y1 > y2) for (let i = 0; i <= x1 - x2; i++) points.push([x1 - i, y1 - i]);
    else throw new Error('Should not be here');
  }
  return points;
}

function getLinePointsWithDiagonal2(x1, y1, x2, y2) {
  let points = [];
  const dx = Math.sign(x2 - x1);
  const dy = Math.sign(y2 - y1);
  for (let x = x1, y = y1; x != x2 + dx || y != y2 + dy; x += dx, y += dy) {
    points.push([x, y])
  }
  return points;
}

function markBoard(board, coord) {
  if (!board[coord]) board[coord] = 0;
  board[coord]++;
}

function printBoard(board) {
  let points = Object.keys(board).map(toPoints);
  let xs = points.map(p => p[0]);
  let ys = points.map(p => p[1]);
  let [minX, maxX] = [Math.min(...xs), Math.max(...xs)];
  let [minY, maxY] = [Math.min(...ys), Math.max(...ys)];
  for (let y = minY; y <= maxY; y++) {
    let line = '';
    for (let x = minX; x <= maxX; x++) {
      let value = board[toCoord([x, y])] === undefined ? '.' : board[toCoord([x, y])];
      line += value;
    }
    console.log(line);
  }
}

let ENTRY_REGEX = /^(\d+),(\d+) -> (\d+),(\d+)/;

const part1 = input => {
  /** @type string[] */
  let lines = input
    .trim()
    .split('\n');

  let board = {};
  for (let line of lines) {
    let [, x1, y1, x2, y2] = line.match(ENTRY_REGEX).map(s => parseInt(s, 10));
    let coords = getLinePoints(x1, y1, x2, y2).map(toCoord);
    for (let c of coords) markBoard(board, c)
  }
  // printBoard(board);
  return Object.values(board).filter(num => num > 1).length;
};

const part2 = input => {
  /** @type string[] */
  let lines = input
    .trim()
    .split('\n');

  let board = {};
  for (let line of lines) {
    let [, x1, y1, x2, y2] = line.match(ENTRY_REGEX).map(s => parseInt(s, 10));
    let coords = getLinePointsWithDiagonal2(x1, y1, x2, y2).map(toCoord);
    for (let c of coords) markBoard(board, c)
  }
  // printBoard(board);
  return Object.values(board).filter(num => num > 1).length;
};

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

// input = `0,9 -> 5,9
// 8,0 -> 0,8
// 9,4 -> 3,4
// 2,2 -> 2,1
// 7,0 -> 7,4
// 6,4 -> 2,0
// 0,9 -> 2,9
// 3,4 -> 1,4
// 0,0 -> 8,8
// 5,5 -> 8,2`

console.log('day5 part1:', part1(input));
console.log('day5 part2:', part2(input));

/*

import {readLines} from "https://deno.land/std/io/mod.ts";

function isPart1() {
    return Deno.args.length == 0 || Deno.args[0] == "1";
}

const grid = new Map();

for await (const l of readLines(Deno.stdin)) {
    const points = l.split(" -> ")
    const [x1, y1] = points[0].split(",").map(x => parseInt(x, 10));
    const [x2, y2] = points[1].split(",").map(x => parseInt(x, 10));

    if (x1 == x2 || y1 == y2 || !isPart1()) {
        const dx = Math.sign(x2 - x1);
        const dy = Math.sign(y2 - y1);
        for (let x = x1, y = y1; x != x2 + dx || y != y2 + dy; x += dx, y += dy) {
            const key = `${x},${y}`;
            grid.set(key, (grid.get(key) ?? 0) + 1);
        }
    }
}

console.log(Array.from(grid.values()).filter(x => x >= 2).length)

*/
