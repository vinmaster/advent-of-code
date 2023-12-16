function transposeStrings(array: string[]) {
  return array[0].split('').map((_, colIndex) => array.map(row => row[colIndex]).join(''));
}

function findReflection(grid: string[]): number {
  let lookup: Record<string, number[]> = {};
  for (let i = 0; i < grid.length; i++) {
    let line = grid[i];
    lookup[line] ??= [];
    if (lookup[line].length === 0) {
      lookup[line].push(i);
    } else {
      // Found possible reflection line
      let valid = true;
      for (let up = i - 1, down = i; up >= 0 && down < grid.length; up--, down++) {
        if (grid[up] !== grid[down]) valid = false;
      }
      if (valid) return i;
    }
  }
  return 0;
}

// String.prototype.replaceAt = function(index, replacement) {
function replaceAt(str, index, replacement) {
  return str.substring(0, index) + replacement + str.substring(index + replacement.length);
}

function flip(str, index) {
  if (str[index] === '.') return replaceAt(str, index, '#');
  else return replaceAt(str, index, '.');
}

// Don't work. Giving too many false positives
function fixSmudgeOld(grid: string[], oldR, oldC): string[] {
  for (let r = 0; r < grid.length; r++) {
    for (let c = 0; c < grid[0].length; c++) {
      let newGrid = [...grid];
      newGrid[r] = flip(newGrid[r], c);
      let rows = findReflection(newGrid);
      let cols = findReflection(transposeStrings(newGrid));
      if ([rows, cols].toString() !== '0,0' && (rows !== oldR || cols !== oldC)) {
        // newGrid.forEach(r => console.log(r));
        // transposeStrings(newGrid).forEach(r => console.log(r));
        // console.log('found', r, c, rows, cols);
        return newGrid;
      }
    }
  }
  return grid;
}

// Return count of how many different cells there are
function diffCount(grid1: string[], grid2: string[]): number {
  let count = 0;
  for (let r = 0; r < grid1.length; r++) {
    for (let c = 0; c < grid1[0].length; c++) {
      if (grid1[r][c] !== grid2[r][c]) count += 1;
    }
  }
  return count;
}

// Doesn't work
function fixSmudgeOld2(grid: string[]): string[] {
  for (let r = 0; r < grid.length; r++) {
    for (let c = 0; c < grid[0].length; c++) {
      let newGrid = [...grid];
      newGrid[r] = flip(newGrid[r], c);
      // Find diff between 2 grids
      if (diffCount(grid, newGrid) === 1) {
        newGrid.forEach(r => console.log(r));
        console.log();
        return newGrid;
      }
    }
  }
  return grid;
}

function findReflectionOnHorizontalWithDiff(grid: string[]): number {
  for (let r = 1; r < grid.length; r++) {
    // Get the top/bottom part ignoring extra rows
    let length = Math.min(r, grid.length - r);
    let top = grid.slice(r - length, r);
    let bottom = grid.slice(r, r + length);
    bottom.reverse();
    if (diffCount(top, bottom) === 1) {
      return r;
    }
  }
  return 0;
}

function part1(input: string) {
  let grids = input
    .trim()
    .split('\n\n')
    .map(grid => grid.split('\n'));
  let sum = 0;
  for (let grid of grids) {
    let rows = findReflection(grid);
    let cols = findReflection(transposeStrings(grid));
    sum += rows * 100;
    if (rows === 0) sum += cols;
  }
  return sum;
}

function part2(input: string) {
  let grids = input
    .trim()
    .split('\n\n')
    .map(grid => grid.split('\n'));
  let sum = 0;
  for (let grid of grids) {
    let rows = findReflectionOnHorizontalWithDiff(grid);
    let cols = findReflectionOnHorizontalWithDiff(transposeStrings(grid));
    sum += rows * 100;
    if (rows === 0) sum += cols;
  }
  return sum;
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
