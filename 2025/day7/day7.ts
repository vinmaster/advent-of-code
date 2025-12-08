type Coord = number[];
type Input = { grid: string[][]; start: Coord };

export function part1(input: Input) {
  let { grid, start } = input;
  let split = 0;
  grid[1][start[1]] = '|';
  for (let row = 1; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      if (grid[row - 1][col] === '|' && grid[row][col] === '^') {
        if (col - 1 >= 0 || col + 1 < grid.length) split += 1;
        if (col - 1 >= 0) {
          grid[row][col - 1] = '|';
        }
        if (col + 1 < grid.length) {
          grid[row][col + 1] = '|';
        }
      }
      if (grid[row][col] !== '^' && grid[row - 1][col] === '|') grid[row][col] = '|';
    }
  }
  for (let line of grid) {
    // console.log(line.join(''));
  }
  console.log('part1:', split);
}

export function part2(input: Input) {
  let { grid, start } = input;
  grid[1][start[1]] = '|';
  let numGrid = Array.from({ length: grid.length }, () => Array(grid[0].length).fill(0));
  numGrid[1][start[1]] = 1;

  for (let row = 1; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      if (grid[row - 1][col] === '|' && grid[row][col] === '^') {
        if (col - 1 >= 0) {
          grid[row][col - 1] = '|';
          numGrid[row][col - 1] += numGrid[row - 1][col];
        }
        if (col + 1 < grid.length) {
          grid[row][col + 1] = '|';
          numGrid[row][col + 1] += numGrid[row - 1][col];
        }
      }
      if (grid[row][col] !== '^' && grid[row - 1][col] === '|') {
        grid[row][col] = '|';
        numGrid[row][col] += numGrid[row - 1][col];
      }
    }
  }
  for (let line of numGrid) {
    // console.log(line.join(','));
  }
  console.log(
    'part2:',
    numGrid.at(-1)!.reduce((x, sum) => x + sum)
  );
}

function parseInput(inputString: string): Input {
  let lines = inputString.trim().split('\n');
  let grid = lines.map(line => line.split(''));
  let start = [0, 0];
  for (let row = 0; row < lines.length; row++) {
    for (let col = 0; col < lines[0].length; col++) {
      if (grid[row][col] === 'S') start = [row, col];
    }
  }
  return { grid, start };
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
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............`;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
  part2(parseInput(inputString));
}

// await main(false);
await main();

/*
Part 2 example
Credit to: https://www.reddit.com/r/adventofcode/comments/1pgj2ad/2025_day_7_part2_python_beam_splitting/

0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
0,0,0,0,0,0,0,1,0,0,0,0,0,0,0
0,0,0,0,0,0,1,0,1,0,0,0,0,0,0
0,0,0,0,0,0,1,0,1,0,0,0,0,0,0
0,0,0,0,0,1,0,2,0,1,0,0,0,0,0
0,0,0,0,0,1,0,2,0,1,0,0,0,0,0
0,0,0,0,1,0,3,0,3,0,1,0,0,0,0
0,0,0,0,1,0,3,0,3,0,1,0,0,0,0
0,0,0,1,0,4,0,3,3,1,0,1,0,0,0
0,0,0,1,0,4,0,3,3,1,0,1,0,0,0
0,0,1,0,5,0,4,3,4,0,2,0,1,0,0
0,0,1,0,5,0,4,3,4,0,2,0,1,0,0
0,1,0,1,5,4,0,7,4,0,2,1,0,1,0
0,1,0,1,5,4,0,7,4,0,2,1,0,1,0
1,0,2,0,10,0,11,0,11,0,2,1,1,0,1
1,0,2,0,10,0,11,0,11,0,2,1,1,0,1
*/
