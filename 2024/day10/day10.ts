type Input = number[][];
const DIR: Coord[] = [
  [-1, 0],
  [0, 1],
  [1, 0],
  [0, -1],
] as const;
type Coord = [number, number];

function addCoords(coord1: Coord, coord2: Coord): Coord {
  return [coord1[0] + coord2[0], coord1[1] + coord2[1]];
}

function isValid(grid: any[][], coord: Coord): boolean {
  return coord[0] >= 0 && coord[0] < grid.length && coord[1] >= 0 && coord[1] < grid[0].length;
}

function takeAHike(grid: number[][], coord: Coord, visited = {} as Record<string, any>) {
  let current = grid[coord[0]][coord[1]];
  if (visited[coord.toString()]) return 0;
  visited[coord.toString()] = true;
  if (current === 9) {
    return 1;
  }
  let neighborCoords = DIR.map(d => addCoords(coord, d)).filter(coord => isValid(grid, coord));
  let scores = 0;
  for (let [r, c] of neighborCoords) {
    // Can only go up by 1
    if (grid[r][c] === current + 1) {
      scores += takeAHike(grid, [r, c], visited);
    }
  }
  return scores;
}

function takeAHikeAllowDups(grid: number[][], coord: Coord) {
  let current = grid[coord[0]][coord[1]];
  if (current === 9) {
    return 1;
  }
  let neighborCoords = DIR.map(d => addCoords(coord, d)).filter(coord => isValid(grid, coord));
  let ratings = 0;
  for (let [r, c] of neighborCoords) {
    // Can only go up by 1
    if (grid[r][c] === current + 1) {
      ratings += takeAHikeAllowDups(grid, [r, c]);
    }
  }
  return ratings;
}

export function part1(input: Input) {
  let grid = input;
  let scores = 0;
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      if (grid[row][col] === 0) {
        scores += takeAHike(grid, [row, col]);
      }
    }
  }
  return scores;
}

export function part2(input: Input) {
  let grid = input;
  let ratings = 0;
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      if (grid[row][col] === 0) {
        ratings += takeAHikeAllowDups(grid, [row, col]);
      }
    }
  }
  return ratings;
}

function parseInput(inputString: string): Input {
  inputString = inputString.trim();
  return inputString
    .trim()
    .split('\n')
    .map(row => row.split('').map(Number));
}

async function main(useRealInput = true) {
  let inputString = '';
  try {
    inputString =
      // @ts-expect-error: next-line
      typeof Bun !== 'undefined'
        ? // @ts-expect-error: next-line
          await Bun.file(`${import.meta.dir}/input.txt`).text()
        : // @ts-expect-error: next-line
        typeof Deno !== 'undefined'
        ? // @ts-expect-error: next-line
          await Deno.readTextFile(`${import.meta.dirname}/input.txt`)
        : '';
  } catch (error) {
    useRealInput = false;
  }

  //   let testInput = `
  // 89010123
  // 78121874
  // 87430965
  // 96549874
  // 45678903
  // 32019012
  // 01329801
  // 10456732`;

  let testInput = `
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732`;

  if (!useRealInput) inputString = testInput;

  let input = parseInput(inputString);
  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// await main(false);
await main();
