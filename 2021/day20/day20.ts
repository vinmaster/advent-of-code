type Input = { algorithm: number[]; grid: Grid };
type Grid = Record<string, number>;
type Vector = [number, number];

const adjacentVectors: Vector[] = [
  [-1, -1],
  [0, -1],
  [1, -1], // Top row
  [-1, 0],
  [0, 0],
  [1, 0], // Middle row
  [-1, 1],
  [0, 1],
  [1, 1], // Bottom row
];

function printImage(grid: Grid, voidValue: 0 | 1) {
  let points = Object.keys(grid).map(str => str.split(',').map(Number));

  if (points.length === 0) {
    console.log(voidValue === 1 ? 'Infinite #' : 'Infinite .');
    return;
  }

  let xs = points.map(p => p[0]);
  let ys = points.map(p => p[1]);
  let [minX, maxX] = [Math.min(...xs), Math.max(...xs)];
  let [minY, maxY] = [Math.min(...ys), Math.max(...ys)];

  const voidChar = voidValue === 1 ? '#' : '.';

  for (let y = minY; y <= maxY; y++) {
    let line = '';
    for (let x = minX; x <= maxX; x++) {
      let pixelValue = grid[[x, y].toString()];
      if (pixelValue === undefined) {
        line += voidChar;
      } else {
        line += pixelValue === 1 ? '#' : '.';
      }
    }
    console.log(line);
  }
}

function add(a: Vector, b: Vector): Vector {
  return [a[0] + b[0], a[1] + b[1]];
}

function enhanceImage(grid: Grid, algorithm: number[], voidValue: 0 | 1): [Grid, 0 | 1] {
  const newVoidValue = (voidValue === 0 ? algorithm[0] : algorithm[511]) as 0 | 1;
  let newGrid: Grid = {};
  let points = Object.keys(grid).map(s => s.split(',').map(Number) as Vector);

  if (points.length === 0) {
    return [newGrid, newVoidValue];
  }

  let xs = points.map(p => p[0]);
  let ys = points.map(p => p[1]);
  let [minX, maxX] = [Math.min(...xs) - 1, Math.max(...xs) + 1];
  let [minY, maxY] = [Math.min(...ys) - 1, Math.max(...ys) + 1];

  for (let y = minY; y <= maxY; y++) {
    for (let x = minX; x <= maxX; x++) {
      const point: Vector = [x, y];
      let code = adjacentVectors
        .map(delta => add(point, delta))
        .map(p => grid[`${p[0]},${p[1]}`] ?? voidValue)
        .join('');

      let index = parseInt(code, 2);
      const newValue = algorithm[index];
      if (newValue !== newVoidValue) {
        newGrid[`${point[0]},${point[1]}`] = newValue;
      }
    }
  }
  return [newGrid, newVoidValue];
}

export function part1(input: Input): number {
  let { grid, algorithm } = input;
  let currentVoidValue: 0 | 1 = 0; // Start with a dark void
  let enhanceCount = 2;

  for (let n = 0; n < enhanceCount; n++) {
    [grid, currentVoidValue] = enhanceImage(grid, algorithm, currentVoidValue);
  }

  if (currentVoidValue === 1) {
    return Infinity;
  }

  return Object.keys(grid).length;
}

export function part2(input: Input) {
  let { grid, algorithm } = input;
  let currentVoidValue: 0 | 1 = 0; // Start with a dark void
  let enhanceCount = 50;

  for (let n = 0; n < enhanceCount; n++) {
    [grid, currentVoidValue] = enhanceImage(grid, algorithm, currentVoidValue);
  }

  if (currentVoidValue === 1) {
    return Infinity;
  }

  return Object.keys(grid).length;
}

function parseInput(inputString: string): Input {
  const [algorithmInput, imageInput] = inputString.trim().split('\n\n');
  const algorithm = algorithmInput
    .replace(/\n/g, '')
    .split('')
    .map(char => (char === '#' ? 1 : 0));

  let grid: Record<string, number> = {};
  let imageLines = imageInput.split('\n').filter(l => l.length > 0);
  for (let y = 0; y < imageLines.length; y++) {
    for (let x = 0; x < imageLines[y].length; x++) {
      if (imageLines[y][x] === '#') {
        grid[`${x},${y}`] = 1;
      }
    }
  }
  return { grid, algorithm };
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
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
