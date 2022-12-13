// deno-lint-ignore-file prefer-const

interface Coord {
  x: number;
  y: number;
}

const DIRS = {
  U: { y: -1, x: 0 },
  D: { y: 1, x: 0 },
  L: { y: 0, x: -1 },
  R: { y: 0, x: 1 },
};

function toComma(coord: Coord) {
  return `${coord.x},${coord.y}`;
}

function toCoords(str: string) {
  let [x, y] = str.split(',').map(Number);
  return { x, y };
}

function newCoord(coord: Coord, delta: Coord): Coord {
  return { x: coord.x + delta.x, y: coord.y + delta.y };
}

function inBound(grid: string[], coord: Coord): boolean {
  return coord.x >= 0 && coord.y >= 0 && coord.x < grid[0].length && coord.y < grid.length;
}

function withinElevation(a: string, b: string, dir: 'up' | 'down'): boolean {
  if (a === 'S') a = 'a';
  else if (a === 'E') a = 'z';
  if (b === 'S') b = 'a';
  else if (b === 'E') b = 'z';

  if (dir === 'up') {
    return a.charCodeAt(0) <= b.charCodeAt(0) + 1;
  } else {
    return a.charCodeAt(0) >= b.charCodeAt(0) - 1;
  }
}

function breadthFirstSearch1(grid: string[], start: Coord) {
  const queue: [Coord, number][] = [[start, 0]];
  const visited = new Set<string>();

  while (queue.length > 0) {
    const [coord, step] = queue.shift()!;

    if (visited.has(toComma(coord))) continue;
    visited.add(toComma(coord));
    if (grid[coord.y][coord.x] === 'E') {
      return step;
    }

    let neighbors = Object.values(DIRS)
      .map(d => newCoord(coord, d))
      .filter(
        c => inBound(grid, c) && withinElevation(grid[c.y][c.x], grid[coord.y][coord.x], 'up')
      );
    let nexts = neighbors.map(coord => [coord, step + 1] as [Coord, number]);
    queue.push(...nexts);
  }
  return Number.MAX_SAFE_INTEGER;
}

function breadthFirstSearch2(grid: string[], start: Coord) {
  const queue: [Coord, number][] = [[start, 0]];
  const visited = new Set<string>();

  while (queue.length > 0) {
    const [coord, step] = queue.shift()!;

    if (visited.has(toComma(coord))) continue;
    visited.add(toComma(coord));
    if (grid[coord.y][coord.x] === 'a') {
      return step;
    }

    let neighbors = Object.values(DIRS)
      .map(d => newCoord(coord, d))
      .filter(
        c => inBound(grid, c) && withinElevation(grid[c.y][c.x], grid[coord.y][coord.x], 'down')
      );
    let nexts = neighbors.map(coord => [coord, step + 1] as [Coord, number]);
    queue.push(...nexts);
  }
  return Number.MAX_SAFE_INTEGER;
}

function part1(input: string) {
  let grid = input.trim().split('\n');
  let start!: Coord;

  grid.forEach((row, y) =>
    row.split('').forEach((cell, x) => {
      if (cell === 'S') start = { x, y };
    })
  );

  return breadthFirstSearch1(grid, start);
}

function part2(input: string) {
  let grid = input.trim().split('\n');
  let end!: Coord;

  grid.forEach((row, y) =>
    row.split('').forEach((cell, x) => {
      if (cell === 'E') end = { x, y };
    })
  );

  return breadthFirstSearch2(grid, end);
}

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// Sabqponm
// abcryxxl
// accszExk
// acctuvwj
// abdefghi`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
