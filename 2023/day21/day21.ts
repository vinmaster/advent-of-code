const DIRS = ['UP', 'DOWN', 'LEFT', 'RIGHT'] as const;
type Direction = (typeof DIRS)[number];
type Coord = [number, number];
// type CoordStr = `${number},${number}`;
type CoordStr = string;
const DIR_DELTAS: Record<Direction, Coord> = {
  UP: [0, -1],
  DOWN: [0, 1],
  LEFT: [-1, 0],
  RIGHT: [1, 0],
};
type Grid = string[][];

function moveCoord(coord: Coord, dir: Direction, distance: number = 1): Coord {
  let [dr, dc] = DIR_DELTAS[dir];
  return [coord[0] + dr * distance, coord[1] + dc * distance];
}

function getIsValid(grid: string[][]) {
  return (coord: Coord): boolean =>
    coord[0] >= 0 && coord[0] < grid.length && coord[1] >= 0 && coord[1] < grid[0].length;
}

function fill(grid: Grid, starting: Coord, steps) {
  let isValid = getIsValid(grid);
  let queue = [starting.toString()];
  for (let step = 0; step < steps; step++) {
    let nextQueue: CoordStr[] = [];
    for (let coordStr of queue) {
      let coord = coordStr.split(',').map(Number) as Coord;
      for (let dir of DIRS) {
        let next = moveCoord(coord, dir);
        if (
          isValid(next) &&
          grid[next[0]][next[1]] !== '#' &&
          !nextQueue.includes(next.toString())
        ) {
          nextQueue.push(next.toString());
        }
      }
    }
    queue = nextQueue;
  }
  // for (let [r, c] of queue) {
  //   grid[r][c] = 'O';
  // }
  // grid.forEach(row => console.log(row.join('')));
  return queue.length;
}

function fill2(grid: Grid, starting: Coord, steps: number) {
  let isValid = getIsValid(grid);
  let ans = new Set<string>();
  let seen = [starting.toString()];
  let queue: [string, number][] = [[starting.toString(), steps]];
  while (queue.length > 0) {
    let [coordStr, step] = queue.shift()!;
    if (step % 2 === 0) ans.add(coordStr);
    if (step === 0) continue;
    let coord = coordStr.split(',').map(Number) as Coord;
    for (let dir of DIRS) {
      let next = moveCoord(coord, dir);
      if (!isValid(next) || grid[next[0]][next[1]] === '#' || seen.includes(next.toString())) {
        continue;
      }
      seen.push(next.toString());
      queue.push([next.toString(), step - 1]);
    }
  }
  return ans.size;
}

function part1(input: string) {
  let grid = input
    .trim()
    .split('\n')
    .map(row => row.split(''));
  let starting: Coord = [0, 0];
  for (let r = 0; r < grid.length; r++) {
    for (let c = 0; c < grid[0].length; c++) {
      if (grid[r][c] === 'S') starting = [r, c];
    }
  }
  return fill2(grid, starting, 64);
}

// Credit: https://www.youtube.com/watch?v=9UOMZSL0JTg
import assert from 'node:assert';
function part2(input: string) {
  let grid = input
    .trim()
    .split('\n')
    .map(row => row.split(''));

  let starting: Coord = [0, 0];
  for (let r = 0; r < grid.length; r++) {
    for (let c = 0; c < grid[0].length; c++) {
      if (grid[r][c] === 'S') starting = [r, c];
    }
  }
  let size = grid.length;
  let steps = 26501365;
  assert.ok(starting[0] === starting[1] && starting[1] === Math.floor(size / 2));
  assert.ok(steps % size === Math.floor(size / 2));

  let gridWidth = Math.floor(steps / size - 1);

  let odd = Math.pow(Math.floor(gridWidth / 2) * 2 + 1, 2);
  let even = Math.pow((Math.floor(gridWidth + 1) / 2) * 2, 2);

  let oddPoints = fill2(grid, starting, size * 2 + 1);
  let evenPoints = fill2(grid, starting, size * 2);

  let cornerTop = fill2(grid, [size - 1, starting[1]], size - 1);
  let cornerRight = fill2(grid, [starting[0], 0], size - 1);
  let cornerBottom = fill2(grid, [0, starting[1]], size - 1);
  let cornerLeft = fill2(grid, [starting[0], size - 1], size - 1);

  let smallTopRight = fill2(grid, [size - 1, 0], Math.floor(size / 2) - 1);
  let smallTopLeft = fill2(grid, [size - 1, size - 1], Math.floor(size / 2) - 1);
  let smallBottomRight = fill2(grid, [0, 0], Math.floor(size / 2) - 1);
  let smallBottomLeft = fill2(grid, [0, size - 1], Math.floor(size / 2) - 1);

  let largeTopRight = fill2(grid, [size - 1, 0], Math.floor((size * 3) / 2) - 1);
  let largeTopLeft = fill2(grid, [size - 1, size - 1], Math.floor((size * 3) / 2) - 1);
  let largeBottomRight = fill2(grid, [0, 0], Math.floor((size * 3) / 2) - 1);
  let largeBottomLeft = fill2(grid, [0, size - 1], Math.floor((size * 3) / 2) - 1);

  return (
    odd * oddPoints +
    even * evenPoints +
    (cornerTop + cornerRight + cornerBottom + cornerLeft) +
    (gridWidth + 1) * (smallTopRight + smallTopLeft + smallBottomRight + smallBottomLeft) +
    gridWidth * (largeTopRight + largeTopLeft + largeBottomRight + largeBottomLeft)
  );
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
