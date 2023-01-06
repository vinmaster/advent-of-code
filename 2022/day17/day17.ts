// deno-lint-ignore-file prefer-const no-explicit-any

let PIECES = [
  [[' ', ' ', '@', '@', '@', '@']],
  [
    [' ', ' ', ' ', '@', ' '],
    [' ', ' ', '@', '@', '@'],
    [' ', ' ', ' ', '@', ' '],
  ],
  [
    [' ', ' ', ' ', ' ', '@'],
    [' ', ' ', ' ', ' ', '@'],
    [' ', ' ', '@', '@', '@'],
  ],
  [
    [' ', ' ', '@'],
    [' ', ' ', '@'],
    [' ', ' ', '@'],
    [' ', ' ', '@'],
  ],
  [
    [' ', ' ', '@', '@'],
    [' ', ' ', '@', '@'],
  ],
];
let DIRS = {
  U: { y: -1, x: 0 },
  D: { y: 1, x: 0 },
  L: { y: 0, x: -1 },
  R: { y: 0, x: 1 },
} as const;
let WIDTH = 7;

function part1(input: string) {
  let jets = input.trim().split('');
  let yGap = 3;
  let yCurrent = 0;
  let grid: string[][] = new Array(yGap).fill(0).map(() => new Array(WIDTH).fill(' '));
  let isRest = true;
  let piecesCount = 0;
  let target = 2022;

  for (let i = 0; piecesCount <= target; i++) {
    let move = jets.at(i % jets.length);
    if (isRest) {
      isRest = false;
      let piece = PIECES[piecesCount % PIECES.length];
      while (yCurrent + yGap + piece.length > grid.length) {
        grid.unshift(new Array(WIDTH).fill(' '));
      }
      for (let index = 0; index < piece.length; index++) {
        let y = grid.length - (yCurrent + yGap + index + 1);
        for (let x = 0; x < piece[index].length; x++) {
          grid[y][x] = piece[piece.length - 1 - index][x];
        }
      }
      piecesCount += 1;
    }
    if (move === '>') {
      moveDir(grid, DIRS.R);
    } else if (move === '<') {
      moveDir(grid, DIRS.L);
    }
    let didMoveDown = moveDir(grid, DIRS.D);
    if (!didMoveDown) {
      isRest = true;
      restCurrentPiece(grid);
      yCurrent = grid.length - grid.findIndex(row => row.join('').includes('#'));
    }
  }
  // print(grid);
  return yCurrent;
}

function part2(input: string) {
  let jets = input.trim().split('');
  let yGap = 3;
  let yCurrent = 0;
  let grid: string[][] = new Array(yGap).fill(0).map(() => new Array(WIDTH).fill(' '));
  let isRest = true;
  let piecesCount = 0;
  let target = 1_000_000_000_000;
  // seen values: counter of seen, pieces count, top
  let seen = new Map<string, number[]>();
  let addedByCycle = 0;

  for (let i = 0; piecesCount <= target; i++) {
    let move = jets.at(i % jets.length);
    if (isRest) {
      isRest = false;
      let piece = PIECES[piecesCount % PIECES.length];
      while (yCurrent + yGap + piece.length > grid.length) {
        grid.unshift(new Array(WIDTH).fill(' '));
      }
      for (let index = 0; index < piece.length; index++) {
        let y = grid.length - (yCurrent + yGap + index + 1);
        for (let x = 0; x < piece[index].length; x++) {
          grid[y][x] = piece[piece.length - 1 - index][x];
        }
      }
      piecesCount += 1;
    }
    if (move === '>') {
      moveDir(grid, DIRS.R);
    } else if (move === '<') {
      moveDir(grid, DIRS.L);
    }
    let didMoveDown = moveDir(grid, DIRS.D);
    if (!didMoveDown) {
      isRest = true;
      restCurrentPiece(grid);
      yCurrent = grid.length - grid.findIndex(row => row.join('').includes('#'));

      // look for cycle
      if (addedByCycle === 0) {
        let key = `${piecesCount % PIECES.length},${i % jets.length}`;
        let entry = seen.get(key);
        if (entry) {
          if (entry[0] >= 2) {
            let deltaY = yCurrent - entry[2];
            let deltaPieceCount = piecesCount - entry[1];
            let repeats = Math.floor((target - piecesCount) / deltaPieceCount);
            addedByCycle += repeats * deltaY;
            piecesCount += repeats * deltaPieceCount;
          }
          seen.set(key, [entry[0] + 1, piecesCount, yCurrent]);
        } else {
          seen.set(key, [1, piecesCount, yCurrent]);
        }
      }
    }
  }
  // print(grid);
  return yCurrent + addedByCycle;
}

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));

function print(grid: string[][]) {
  for (let y = 0; y < grid.length; y++) {
    console.log('|' + grid[y].join('') + '|');
  }
  console.log('+'.padEnd(WIDTH + 1, '-') + '+');
}

function inBound(grid: string[][], x: number, y: number): boolean {
  return x >= 0 && y >= 0 && x < grid[0].length && y < grid.length;
}

function moveDir(grid: string[][], { y: dy, x: dx }: { y: number; x: number }) {
  let canMove = true;
  for (let y = grid.length - 1; y > 0; y--) {
    for (let x = 0; x < grid[0].length; x++) {
      if (
        grid[y][x] === '@' &&
        (!inBound(grid, x + dx, y + dy) || ['#'].includes(grid[y + dy][x + dx]))
      ) {
        canMove = false;
      }
    }
  }
  if (canMove) {
    for (let y = grid.length - 1; y >= 0; y--) {
      if (dx >= 0) {
        for (let x = grid[0].length - 1; x >= 0; x--) {
          if (grid[y][x] === '@') {
            grid[y + dy][x + dx] = '@';
            grid[y][x] = ' ';
          }
        }
      } else if (dx < 0) {
        for (let x = 0; x < grid[0].length; x++) {
          if (grid[y][x] === '@') {
            grid[y + dy][x + dx] = '@';
            grid[y][x] = ' ';
          }
        }
      }
    }
  }
  return canMove;
}

function restCurrentPiece(grid: string[][]) {
  for (let y = grid.length - 1; y > 0; y--) {
    for (let x = 0; x < grid[0].length; x++) {
      if (grid[y][x] === '@') grid[y][x] = '#';
    }
  }
}
