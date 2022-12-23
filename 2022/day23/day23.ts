// deno-lint-ignore-file prefer-const no-explicit-any

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// ....#..
// ..###.#
// #...#.#
// .#...##
// #.###..
// ##.#.##
// .#..#..`;

const DIRS: Coord[][] = [
  [
    [0, -1],
    [-1, -1],
    [1, -1],
  ],
  [
    [0, 1],
    [-1, 1],
    [1, 1],
  ],
  [
    [-1, 0],
    [-1, -1],
    [-1, 1],
  ],
  [
    [1, 0],
    [1, -1],
    [1, 1],
  ],
];

console.log('part1:', part1(input));
console.log('part2:', part2(input));

function part1(input: string) {
  let lines = input.trim().split('\n');
  let grid = createGrid(lines);

  for (let round = 0; round < 10; round++) {
    grid = simulate(grid, round);
  }
  // print(grid);

  let points = (Object.keys(grid) as CoordStr[]).map(toCoord);
  let xs = points.map(p => p[0]);
  let ys = points.map(p => p[1]);
  let [minX, maxX] = [Math.min(...xs), Math.max(...xs)];
  let [minY, maxY] = [Math.min(...ys), Math.max(...ys)];
  let count = 0;
  for (let y = minY; y <= maxY; y++) {
    for (let x = minX; x <= maxX; x++) {
      if (grid[toStr([x, y])] === undefined) {
        count += 1;
      }
    }
  }
  return count;
}

function part2(input: string) {
  let lines = input.trim().split('\n');
  let grid = createGrid(lines);

  for (let round = 0; ; round++) {
    let next = simulate(grid, round);
    if (JSON.stringify(next) === JSON.stringify(grid)) return round + 1;
    grid = next;
  }
  // print(grid);
}

type Coord = [number, number];
type CoordStr = `${number},${number}`;

function simulate(grid: Record<CoordStr, string>, round: number): Record<CoordStr, string> {
  let nextGrid = {} as Record<CoordStr, string>;
  for (let curStr of Object.keys(grid) as CoordStr[]) {
    let cur = toCoord(curStr);
    let dirs = [
      ...DIRS.slice(round % DIRS.length, DIRS.length),
      ...DIRS.slice(0, round % DIRS.length),
    ];
    let moveDirs = dirs.filter(ds =>
      ds.every(d => {
        let c = addCoord(toCoord(curStr), d);
        return grid[toStr(c)] !== '#';
      })
    );
    if (moveDirs.length === DIRS.length) {
      nextGrid[curStr] = curStr;
    } else if (moveDirs.length > 0) {
      let dir = moveDirs[0][0];
      let nextStr = toStr(addCoord(cur, dir));
      if (nextGrid[nextStr]) {
        nextGrid[nextStr] += `|${curStr}`;
      } else {
        nextGrid[nextStr] = curStr;
      }
      let index = dirs.indexOf(moveDirs[0]);
      dirs.push(dirs.splice(index, 1) as any);
    } else {
      nextGrid[curStr] = curStr;
    }
  }
  for (let nextStr of Object.keys(nextGrid) as CoordStr[]) {
    let coordStrs = nextGrid[nextStr].split('|') as CoordStr[];
    if (coordStrs.length === 1) {
      nextGrid[nextStr] = '#';
    } else {
      // move back
      for (let coordStr of coordStrs) {
        nextGrid[coordStr] = '#';
      }
      delete nextGrid[nextStr];
    }
  }
  return nextGrid;
}

function createGrid(lines: string[]) {
  let grid = {} as Record<CoordStr, string>;
  for (let y = 0; y < lines.length; y++) {
    for (let x = 0; x < lines[y].length; x++) {
      if (lines[y][x] === '#') grid[`${x},${y}`] = lines[y][x];
    }
  }
  return grid;
}

function toStr(coord: Coord): CoordStr {
  return coord.toString() as CoordStr;
}

function toCoord(str: CoordStr): Coord {
  return str.split(',').map(Number) as Coord;
}

function addCoord(c1: Coord, c2: Coord): Coord {
  return [c1[0] + c2[0], c1[1] + c2[1]];
}

function print(grid: Record<CoordStr, string>) {
  let points = (Object.keys(grid) as CoordStr[]).map(toCoord);
  let xs = points.map(p => p[0]);
  let ys = points.map(p => p[1]);
  let [minX, maxX] = [Math.min(...xs), Math.max(...xs)];
  let [minY, maxY] = [Math.min(...ys), Math.max(...ys)];
  for (let y = minY; y <= maxY; y++) {
    let line = '';
    for (let x = minX; x <= maxX; x++) {
      let value = grid[toStr([x, y])] === undefined ? '.' : grid[toStr([x, y])];
      line += value;
    }
    console.log(line);
  }
}
