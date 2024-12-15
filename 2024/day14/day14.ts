type Grid = Record<string, number[][]>;
let SIZE = [0, 0];

// function elapseTime(grid: Grid, seconds: number) {
//   let newGrid = {} as Grid;
//   for (let coordStr of Object.keys(grid)) {
//     let coord = coordStr.split(',').map(Number);
//     for (let [vx, vy] of grid[coordStr]) {
//       let newCoord = [
//         (coord[0] + (vx + SIZE[0]) * seconds) % SIZE[0],
//         (coord[1] + (vy + SIZE[1]) * seconds) % SIZE[1],
//       ];
//       newGrid[newCoord.toString()] ??= [];
//       newGrid[newCoord.toString()].push([vx, vy]);
//     }
//   }
//   return newGrid;
// }

function elapseTime(grid: Grid) {
  let newGrid = {} as Grid;
  for (let coordStr of Object.keys(grid)) {
    let coord = coordStr.split(',').map(Number);
    for (let [vx, vy] of grid[coordStr]) {
      let newCoord = [(coord[0] + vx + SIZE[0]) % SIZE[0], (coord[1] + vy + SIZE[1]) % SIZE[1]];
      newGrid[newCoord.toString()] ??= [];
      newGrid[newCoord.toString()].push([vx, vy]);
    }
  }
  return newGrid;
}

function printGrid(grid) {
  let points = Object.keys(grid).map(str => str.split(',').map(Number));
  let xs = points.map(p => p[0]);
  let ys = points.map(p => p[1]);
  let [minX, maxX] = [Math.min(...xs), Math.max(...xs)];
  let [minY, maxY] = [Math.min(...ys), Math.max(...ys)];
  for (let y = minY; y <= maxY; y++) {
    let line = '';
    for (let x = minX; x <= maxX; x++) {
      let value = grid[[x, y].toString()] === undefined ? '.' : '#';
      line += value;
    }
    console.log(line);
  }
}

export function part1(grid: Grid) {
  for (let i = 0; i < 100; i++) grid = elapseTime(grid);
  let midX = Math.floor(SIZE[0] / 2);
  let midY = Math.floor(SIZE[1] / 2);
  let quadrants = [0, 0, 0, 0];
  for (let coordStr of Object.keys(grid)) {
    let coord = coordStr.split(',').map(Number);
    if (coord[0] < midX && coord[1] < midY) quadrants[0] += grid[coordStr].length;
    else if (coord[0] > midX && coord[1] < midY) quadrants[1] += grid[coordStr].length;
    else if (coord[0] < midX && coord[1] > midY) quadrants[2] += grid[coordStr].length;
    else if (coord[0] > midX && coord[1] > midY) quadrants[3] += grid[coordStr].length;
  }
  return quadrants.reduce((a, b) => a * b);
}

export function part2(grid: Grid) {
  for (let i = 1; i <= 10000; i++) {
    grid = elapseTime(grid);
    let coords = Object.keys(grid).map(str => str.split(',').map(Number));
    let Xgroups = Object.groupBy(coords, c => c[0]) as Record<string, number[][]>;
    let Ygroups = Object.groupBy(coords, c => c[1]) as Record<string, number[][]>;
    // Find a time where scan line have over 30 entries in X and Y direction
    if (
      Object.values(Xgroups).filter(line => line.length > 30).length > 1 &&
      Object.values(Ygroups).filter(line => line.length > 30).length > 1
    ) {
      // printGrid(grid);
      return i;
    }
  }
}

function parseInput(inputString: string): Grid {
  if (inputString.length < 200) {
    SIZE = [11, 7];
  } else {
    SIZE = [101, 103];
  }
  return inputString
    .trim()
    .split('\n')
    .map(line => {
      let [x, y, vx, vy] = line
        .match(/.*p=(\d+),(\d+) v=(-?\d+),(-?\d+).*/)!
        .slice(1)
        .map(Number);
      return [x, y, vx, vy];
    })
    .reduce((grid, [x, y, vx, vy]) => {
      let coordStr = [x, y].toString();
      grid[coordStr] ??= [];
      grid[coordStr].push([vx, vy]);
      return grid;
    }, {} as Grid);
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

  let testInput = `
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
`;

  if (!useRealInput) inputString = testInput;

  let input = parseInput(inputString);
  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// await main(false);
await main();
