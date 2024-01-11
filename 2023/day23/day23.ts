const DIRS = ['UP', 'DOWN', 'LEFT', 'RIGHT'] as const;
type Direction = (typeof DIRS)[number];
type Coord = [number, number];
type CoordStr = string;
const DIR_DELTAS: Record<Direction, Coord> = {
  UP: [0, -1],
  DOWN: [0, 1],
  LEFT: [-1, 0],
  RIGHT: [1, 0],
};
type Grid = string[][];

function* enumerate<T>(xs: Iterable<T>): IterableIterator<[number, T]> {
  let i = 0;
  for (const x of xs) {
    yield [i++, x];
  }
}

function getIsValid(grid: Grid) {
  return (coord: Coord): boolean =>
    coord[0] >= 0 && coord[0] < grid.length && coord[1] >= 0 && coord[1] < grid[0].length;
}

function moveCoord(coord: Coord, dir: Direction, distance: number = 1): Coord {
  let [dr, dc] = DIR_DELTAS[dir];
  return [coord[0] + dr * distance, coord[1] + dc * distance];
}

function getNeighbors(coord: Coord): Coord[] {
  return DIRS.map(dir => moveCoord(coord, dir));
}

function part1(input: string) {
  let grid: Grid = input
    .trim()
    .split('\n')
    .map(row => row.split(''));
  let isValid = getIsValid(grid);

  let start: Coord = [0, grid[0].findIndex(x => x === '.')];
  let end: Coord = [grid.length - 1, grid.at(-1).findIndex(x => x === '.')];
  let points: CoordStr[] = [start.toString(), end.toString()];

  for (let [r, row] of enumerate(grid)) {
    for (let [c, cell] of enumerate(row)) {
      if (cell === '#') continue;
      let neighbors = 0;
      getNeighbors([r, c])
        .filter(([nr, nc]) => isValid([nr, nc]) && grid[nr][nc] !== '#')
        .forEach(() => (neighbors += 1));
      if (neighbors >= 3) {
        points.push([r, c].toString());
      }
    }
  }
  let dirs: Record<string, Coord[]> = {
    '^': [[-1, 0]],
    v: [[1, 0]],
    '<': [[0, -1]],
    '>': [[0, 1]],
    '.': Object.values(DIR_DELTAS),
  };
  let graph: Record<CoordStr, Record<CoordStr, number>> = {};
  for (let p of points) {
    graph[p] = {};
  }
  for (let p of points) {
    let [sr, sc] = p.split(',');
    let stack: [number, string, string][] = [[0, sr, sc]];
    let seen = new Set();
    seen.add(p);
    while (stack.length > 0) {
      let [n, r, c] = stack.pop()!.map(Number);

      if (n !== 0 && points.includes([r, c].toString())) {
        graph[p][[r, c].toString()] = n;
        continue;
      }
      for (let [dr, dc] of dirs[grid[r][c]]) {
        let [nr, nc] = [r + dr, c + dc];
        if (isValid([nr, nc]) && grid[nr][nc] !== '#' && !seen.has([nr, nc].toString())) {
          stack.push([n + 1, nr.toString(), nc.toString()]);
          seen.add([nr, nc].toString());
        }
      }
    }
  }
  let seen = new Set<CoordStr>();
  let dfs = (p: CoordStr) => {
    if (p === end.toString()) return 0;
    let m = Number.NEGATIVE_INFINITY;
    seen.add(p);
    for (let nx of Object.keys(graph[p])) {
      if (!seen.has(nx)) {
        m = Math.max(m, dfs(nx) + graph[p][nx]);
      }
    }
    seen.delete(p);
    return m;
  };
  return dfs(start.toString());
}

function part2(input: string) {
  let grid: Grid = input
    .trim()
    .split('\n')
    .map(row => row.split(''));
  let isValid = getIsValid(grid);

  let start: Coord = [0, grid[0].findIndex(x => x === '.')];
  let end: Coord = [grid.length - 1, grid.at(-1).findIndex(x => x === '.')];
  let points: CoordStr[] = [start.toString(), end.toString()];

  for (let [r, row] of enumerate(grid)) {
    for (let [c, cell] of enumerate(row)) {
      if (cell === '#') continue;
      let neighbors = 0;
      getNeighbors([r, c])
        .filter(([nr, nc]) => isValid([nr, nc]) && grid[nr][nc] !== '#')
        .forEach(() => (neighbors += 1));
      if (neighbors >= 3) {
        points.push([r, c].toString());
      }
    }
  }
  let graph: Record<CoordStr, Record<CoordStr, number>> = {};
  for (let p of points) {
    graph[p] = {};
  }
  for (let p of points) {
    let [sr, sc] = p.split(',');
    let stack: [number, string, string][] = [[0, sr, sc]];
    let seen = new Set();
    seen.add(p);
    while (stack.length > 0) {
      let [n, r, c] = stack.pop()!.map(Number);

      if (n !== 0 && points.includes([r, c].toString())) {
        graph[p][[r, c].toString()] = n;
        continue;
      }
      for (let [dr, dc] of Object.values(DIR_DELTAS)) {
        let [nr, nc] = [r + dr, c + dc];
        if (isValid([nr, nc]) && grid[nr][nc] !== '#' && !seen.has([nr, nc].toString())) {
          stack.push([n + 1, nr.toString(), nc.toString()]);
          seen.add([nr, nc].toString());
        }
      }
    }
  }
  let seen = new Set<CoordStr>();
  let dfs = (p: CoordStr) => {
    if (p === end.toString()) return 0;
    let m = Number.NEGATIVE_INFINITY;
    seen.add(p);
    for (let nx of Object.keys(graph[p])) {
      if (!seen.has(nx)) {
        m = Math.max(m, dfs(nx) + graph[p][nx]);
      }
    }
    seen.delete(p);
    return m;
  };
  return dfs(start.toString());
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
