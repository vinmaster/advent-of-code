// deno-lint-ignore-file prefer-const

const getHorizontalWords = (grid: string[][], target: string, row: number, col: number) => {
  let horizontal = '';
  for (let i = 0; i < target.length; i++) {
    if (col + i >= grid[0].length) break;
    horizontal += grid[row][col + i];
  }
  return [horizontal, horizontal.split('').reverse().join('')];
};

const getVerticalWords = (grid: string[][], target: string, row: number, col: number) => {
  let vertical = '';
  for (let i = 0; i < target.length; i++) {
    if (row + i >= grid.length) break;
    vertical += grid[row + i][col];
  }
  return [vertical, vertical.split('').reverse().join('')];
};

const getBackSlashWords = (grid: string[][], target: string, row: number, col: number) => {
  let backSlash = '';
  for (let i = 0; i < target.length; i++) {
    if (col + i < 0 || col + i >= grid[0].length) break;
    else if (row + i < 0 || row + i >= grid.length) break;
    backSlash += grid[row + i][col + i];
  }
  return [backSlash, backSlash.split('').reverse().join('')];
};

const getForwardSlashWords = (grid: string[][], target: string, row: number, col: number) => {
  let forwardSlash = '';
  for (let i = 0; i < target.length; i++) {
    if (col - i < 0) break;
    else if (row + i >= grid.length) break;
    forwardSlash += grid[row + i][col - i];
  }
  return [forwardSlash, forwardSlash.split('').reverse().join('')];
};

export function part1(input: string) {
  input = input.trim();
  let target = 'XMAS';
  let grid = input.split('\n').map(line => line.split(''));
  let matchTarget = (word: string) => word === target;
  let count = 0;
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      // Sweep only looking downwards to prevent double counts
      count += getHorizontalWords(grid, target, row, col).filter(matchTarget).length;
      count += getVerticalWords(grid, target, row, col).filter(matchTarget).length;
      count += getBackSlashWords(grid, target, row, col).filter(matchTarget).length;
      count += getForwardSlashWords(grid, target, row, col).filter(matchTarget).length;
    }
  }
  return count;
}

export function part2(input: string) {
  input = input.trim();
  let target = 'MAS';
  let grid = input.split('\n').map(line => line.split(''));
  let matchTarget = (word: string) => word === target;
  let count = 0;

  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      // Center of X
      if (
        getBackSlashWords(grid, target, row - 1, col - 1).find(matchTarget) &&
        getForwardSlashWords(grid, target, row - 1, col + 1).find(matchTarget)
      ) {
        count += 1;
      }
    }
  }
  return count;
}

async function main(useRealInput = true) {
  let input =
    // @ts-ignore: next-line
    typeof Bun !== 'undefined'
      ? // @ts-ignore: next-line
        await Bun.file(`${import.meta.dir}/input.txt`).text()
      : typeof Deno !== 'undefined'
      ? await Deno.readTextFile(`${import.meta.dirname}/input.txt`)
      : '';

  let testInput = `
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
  `;

  if (!useRealInput) input = testInput;

  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// main(false);
main();
