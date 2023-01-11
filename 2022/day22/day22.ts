// deno-lint-ignore-file prefer-const

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `        ...#
//         .#..
//         #...
//         ....
// ...#.......#
// ........#...
// ..#....#....
// ..........#.
//         ...#....
//         .....#..
//         .#......
//         ......#.

// 10R5L5R10L4R5L5
// `;

const DIRS: Coord[] = [
  [0, 1],
  [1, 0],
  [0, -1],
  [-1, 0],
];
const [R, D, L, U] = [0, 1, 2, 3] as const;
type Dir = 0 | 1 | 2 | 3;
let ARROWS = ['>', 'v', '<', '^'];

console.log('part1:', part1(input));
console.log('part2:', part2(input));

function part1(input: string) {
  let [gridStr, pathStr] = input.split('\n\n');
  let grid = parseGrid(gridStr);
  let path = [...pathStr.matchAll(/(\d+)|([RDLU])/g)].map(([_, num, letter]) => {
    if (letter === undefined) return Number(num);
    else if (num === undefined) return letter;
  });
  let cur = startingCoord(grid);
  let dir = 0 as Dir;
  for (let step of path) {
    if (typeof step === 'number') {
      [cur, dir] = move(grid, cur, dir, step, '2d');
    } else if (step === 'R') {
      dir = ((dir + 1) % DIRS.length) as Dir;
    } else if (step === 'L') {
      dir = ((dir - 1 + DIRS.length) % DIRS.length) as Dir;
    }
  }
  grid[cur[0]][cur[1]] = ARROWS[dir];
  // print(grid);
  return 1000 * (cur[0] + 1) + 4 * (cur[1] + 1) + dir;
}

function part2(input: string) {
  let [gridStr, pathStr] = input.split('\n\n');
  let grid = parseGrid(gridStr);
  let path = [...pathStr.matchAll(/(\d+)|([RDLU])/g)].map(([_, num, letter]) => {
    if (letter === undefined) return Number(num);
    else if (num === undefined) return letter;
  });
  let cur = startingCoord(grid);
  let dir = 0 as Dir;
  for (let step of path) {
    if (typeof step === 'number') {
      [cur, dir] = move(grid, cur, dir, step, '3d');
    } else if (step === 'R') {
      dir = ((dir + 1) % DIRS.length) as Dir;
    } else if (step === 'L') {
      dir = ((dir - 1 + DIRS.length) % DIRS.length) as Dir;
    }
  }
  grid[cur[0]][cur[1]] = ARROWS[dir];
  // print(grid);
  return 1000 * (cur[0] + 1) + 4 * (cur[1] + 1) + dir;
}

type Coord = [number, number];

function parseGrid(map: string): string[][] {
  return map.split('\n').map(line => line.split(''));
}

function print(grid: string[][]) {
  for (let row = 0; row < grid.length; row++) {
    let line = '';
    for (let col = 0; col < grid[row].length; col++) {
      line += grid[row][col] || ' ';
    }
    console.log(line);
  }
}

function startingCoord(grid: string[][]): Coord {
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[row].length; col++) {
      if (grid[row][col] === '.') return [row, col];
    }
  }
  return [0, 0];
}

function addCoord(c1: Coord, c2: Coord): Coord {
  return [c1[0] + c2[0], c1[1] + c2[1]];
}

function move(
  grid: string[][],
  cur: Coord,
  dir: Dir,
  step: number,
  wrap: '2d' | '3d'
): [Coord, Dir] {
  for (let i = 0; i < step; i++) {
    let next = addCoord(cur, DIRS[dir]);
    let [row, col] = next;
    if (!inBound(grid, next)) {
      if (wrap === '2d') {
        [cur, dir] = wrap2d(grid, cur, dir);
      } else {
        [cur, dir] = wrap3d(grid, cur, dir);
      }
      continue;
    } else if (grid[row][col] === '#') {
      break;
    }
    grid[cur[0]][cur[1]] = ARROWS[dir];
    cur = next;
  }
  return [cur, dir];
}

function wrap2d(grid: string[][], cur: Coord, dir: Dir): [Coord, Dir] {
  let next = addCoord(cur, DIRS[dir]);
  let [row, col] = next;
  if (dir === R) {
    for (let col = 0; col < grid[0].length; col++) {
      if (grid[row][col] === '#') break;
      else if (grid[row][col] === ' ' || grid[row][col] === undefined) continue;
      grid[cur[0]][cur[1]] = ARROWS[dir];
      cur = [row, col];
      break;
    }
  } else if (dir === D) {
    for (let row = 0; row < grid.length; row++) {
      if (grid[row][col] === '#') break;
      else if (grid[row][col] === ' ' || grid[row][col] === undefined) continue;
      grid[cur[0]][cur[1]] = ARROWS[dir];
      cur = [row, col];
      break;
    }
  } else if (dir === L) {
    for (let col = grid[0].length - 1; col >= 0; col--) {
      if (grid[row][col] === '#') break;
      else if (grid[row][col] === ' ' || grid[row][col] === undefined) continue;
      grid[cur[0]][cur[1]] = ARROWS[dir];
      cur = [row, col];
      break;
    }
  } else if (dir === U) {
    for (let row = grid.length - 1; row >= 0; row--) {
      if (grid[row][col] === '#') break;
      else if (grid[row][col] === ' ' || grid[row][col] === undefined) continue;
      grid[cur[0]][cur[1]] = ARROWS[dir];
      cur = [row, col];
      break;
    }
  }
  return [cur, dir];
}

function wrap3d(grid: string[][], cur: Coord, dir: Dir): [Coord, Dir] {
  // flat cube is in a 4x3 or 3x4
  let size = Math.min(grid.length, grid[0].length) / 3;
  let rules = getRules(size);
  let regions = rules[dir];

  grid[cur[0]][cur[1]] = ARROWS[dir];
  let [_, newDir, f] = regions[getRegion(grid, cur)];
  let newCoord = f(cur);
  if (grid[newCoord[0]][newCoord[1]] === '#' || !inBound(grid, newCoord)) return [cur, dir];
  return [newCoord, newDir];
}

function getRegion(grid: string[][], c: Coord): number {
  let size = Math.min(grid.length, grid[0].length) / 3;
  let cols = Math.max(grid[0].length, grid.at(-1)!.length) / size;
  return Math.floor(c[0] / size) * cols + Math.floor(c[1] / size);
}

/*
example:
      [2]
[4][5][6]
      [10][11]

input file:
   [1][2]
   [4]
[6][7]
[9]
*/
function getRules(size: number) {
  let rules = {} as Record<Dir, Record<number, [number, Dir, (c: Coord) => Coord]>>;
  if (size === 4) {
    // example
    rules = {
      [R]: {
        2: [11, L, c => [size * 3 - c[0] - 1, size * 4 - 1]],
        6: [11, D, c => [size * 2, size * 4 - (c[0] - size) - 1]],
        11: [2, L, c => [size * 3 - c[0] - 1, size * 3 - 1]],
      },
      [D]: {
        4: [10, U, c => [size * 3 - 1, size * 3 - c[1] - 1]],
        5: [10, R, c => [size * 3 - (size - c[1]) - 1, size * 2]],
        10: [4, U, c => [size * 2 - 1, c[1] - size * 2 - 1]],
        11: [4, R, c => [size * 3 - c[1] - 1, 0]],
      },
      [L]: {
        2: [5, D, c => [size, size + c[0]]],
        4: [11, U, c => [size * 4 - c[0] - 1, size * 3 - 1]],
        10: [5, U, c => [size * 2 - 1, size * 2 - (size * 2 - c[0]) - 1]],
      },
      [U]: {
        4: [2, D, c => [0, size * 3 - c[1] - 1]],
        5: [2, R, c => [c[1] - size, size * 2]],
        2: [4, D, c => [size, size - (c[1] - size * 2)]],
        11: [6, L, c => [size * 2 - (c[1] - size * 3), size * 3 - 1]],
      },
    };
  } else {
    // input file
    rules = {
      [R]: {
        2: [7, L, c => [size * 3 - c[0] - 1, size * 2 - 1]],
        4: [2, U, c => [size - 1, c[0] - size + size * 2]],
        7: [2, L, c => [size - (c[0] - size * 2) - 1, size * 3 - 1]],
        9: [7, U, c => [size * 3 - 1, c[0] - size * 3 + size]],
      },
      [D]: {
        9: [2, D, c => [0, c[1] + size * 2]],
        7: [9, L, c => [c[1] - size + size * 3, size - 1]],
        2: [4, L, c => [c[1] - size * 2 + size, size * 2 - 1]],
      },
      [L]: {
        1: [6, R, c => [size - (c[0] - size * 2) - 1, 0]],
        4: [6, D, c => [size * 2, c[0] - size]],
        6: [1, R, c => [size - (c[0] - size * 2) - 1, size]],
        9: [1, D, c => [0, c[0] - size * 3 + size]],
      },
      [U]: {
        6: [4, R, c => [size + c[1], size]],
        1: [9, R, c => [c[1] - size + size * 3, 0]],
        2: [9, U, c => [size * 4 - 1, c[1] - size * 2]],
      },
    };
  }
  return rules;
}

function inBound(grid: string[][], c: Coord): boolean {
  return (
    c[0] >= 0 &&
    c[0] < grid.length &&
    c[1] >= 0 &&
    c[1] < grid[c[0]].length &&
    grid[c[0]][c[1]] !== ' ' &&
    grid[c[0]][c[1]] !== undefined
  );
}
