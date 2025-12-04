type Input = string[][];

const DIRS = [
  [-1, -1],
  [-1, 0],
  [-1, 1],
  [0, -1],
  [0, 1],
  [1, -1],
  [1, 0],
  [1, 1],
];

function isValid(grid: string[][], row: number, col: number): boolean {
  return row >= 0 && col >= 0 && row < grid.length && col < grid[0].length;
}

function removeRoll(grid: string[][]) {
  let count = 0;
  let copy = JSON.parse(JSON.stringify(grid));
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      if (grid[row][col] === '@') {
        let coords = DIRS.map(([r, c]) => [r + row, c + col]).filter(coord =>
          isValid(grid, coord[0], coord[1])
        );
        if (
          coords.map(coord => grid[coord[0]][coord[1]]).filter(value => value === '@').length < 4
        ) {
          count += 1;
          copy[row][col] = '.';
        }
      }
    }
  }
  return [copy, count];
}

export function part1(input: Input) {
  let grid = input;
  let count = 0;

  count += removeRoll(grid)[1];

  console.log('part1:', count);
}

export function part2(input: Input) {
  let grid = input;
  let count = 0;
  let newCount = -1;

  while (newCount !== 0) {
    [grid, newCount] = removeRoll(grid);
    count += newCount;
  }

  console.log('part2:', count);
}

function parseInput(inputString: string): Input {
  let lines = inputString.trim().split('\n');
  let rows = lines.length;
  let cols = lines[0].length;
  let grid = Array.from({ length: rows }, () => Array(cols).fill('.'));
  for (let row = 0; row < rows; row++) {
    for (let col = 0; col < cols; col++) {
      grid[row][col] = lines[row][col];
    }
  }
  return grid;
}

async function main(useRealInput = true) {
  let inputString = '';
  try {
    // @ts-expect-error: next-line
    inputString = await Bun.file(`${import.meta.dir}/input.txt`).text();
  } catch (error) {
    useRealInput = false;
  }

  let testInput = `
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.`;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
  part2(parseInput(inputString));
}

// await main(false);
await main();
