const DIR = ['^', '>', 'v', '<'] as const;
type Direction = (typeof DIR)[number];
type Coord = [number, number];
type CoordStr = string;
const DIR_DELTA: Record<Direction, Coord> = {
  '^': [-1, 0],
  '>': [0, 1],
  v: [1, 0],
  '<': [0, -1],
};

function moveCoord(coord: Coord, dir: Direction): Coord {
  let [dr, dc] = DIR_DELTA[dir];
  return [coord[0] + dr, coord[1] + dc];
}

function findStart(grid: string[][]) {
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      let index = DIR.indexOf(grid[row][col] as Direction);
      if (index !== -1) {
        return [[row, col] as Coord, grid[row][col] as Direction];
      }
    }
  }
}

function isValid(grid: string[][], coord: Coord): boolean {
  return coord[0] >= 0 && coord[0] < grid.length && coord[1] >= 0 && coord[1] < grid[0].length;
}

function exitedGrid(grid, current, dir): [boolean, string[]] {
  let visited = [`${current.toString()}-${dir}`] as string[];

  while (true) {
    let nextCoord = moveCoord(current, dir);
    if (!isValid(grid, nextCoord)) {
      return [true, visited];
    }
    if (grid[nextCoord[0]][nextCoord[1]] === '#') {
      dir = DIR[(DIR.indexOf(dir) + 1) % DIR.length];
    } else {
      current = nextCoord;
      if (visited.includes(`${current.toString()}-${dir}`)) return [false, visited];
      else visited.push(`${current.toString()}-${dir}`);
    }
  }
}

export function part1(input: string) {
  input = input.trim();
  let grid = input.split('\n').map(line => line.split(''));
  let [current, dir] = findStart(grid) as [Coord, Direction];
  let visited = [current.toString()] as string[];

  while (true) {
    let nextCoord = moveCoord(current, dir);
    if (!isValid(grid, nextCoord)) {
      return visited.length;
    }
    if (grid[nextCoord[0]][nextCoord[1]] === '#') {
      dir = DIR[(DIR.indexOf(dir) + 1) % DIR.length];
    } else {
      current = nextCoord;
      if (!visited.includes(current.toString())) visited.push(current.toString());
    }
  }
}

export function part2(input: string) {
  input = input.trim();
  let grid = input.split('\n').map(line => line.split(''));
  let [current, dir] = findStart(grid) as [Coord, Direction];
  let [_, visited] = exitedGrid(grid, current, dir);
  let obstructionCoords: string[] = [];

  for (let v of visited) {
    let [coordStr, _] = v.split('-') as [CoordStr, Direction];
    let obstructionCoord = coordStr.split(',').map(Number) as Coord;
    let newGrid = input.split('\n').map(line => line.split(''));
    // Already tried
    if (obstructionCoords.includes(obstructionCoord.toString())) continue;
    // Cannot be at starting
    if (current.toString() === obstructionCoord.toString()) continue;
    newGrid[obstructionCoord[0]][obstructionCoord[1]] = '#';
    if (!exitedGrid(newGrid, current, dir)[0]) {
      obstructionCoords.push(obstructionCoord.toString());
    }
  }
  return obstructionCoords.length;
}

async function main(useRealInput = true) {
  let input = '';
  try {
    input =
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
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
`;
  if (!useRealInput) input = testInput;

  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// await main(false);
await main();
