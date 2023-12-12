type Coord = [number, number];

const DIRS: Coord[] = [
  [0, 1],
  [0, -1],
  [1, 0],
  [-1, 0],
];
const PIPES: Record<string, Coord[]> = {
  '|': [
    [1, 0],
    [-1, 0],
  ],
  '-': [
    [0, 1],
    [0, -1],
  ],
  L: [
    [-1, 0],
    [0, 1],
  ],
  J: [
    [-1, 0],
    [0, -1],
  ],
  '7': [
    [1, 0],
    [0, -1],
  ],
  F: [
    [1, 0],
    [0, 1],
  ],
};
let grid = [] as string[];

function isValid([row, col]: Coord): boolean {
  return row >= 0 && row < grid.length && col >= 0 && col < grid[0].length;
}

function notVisited(visited: Coord[], [r, c]: Coord): boolean {
  return !visited.some(([vr, vc]) => vr === r && vc === c);
}

function isConnected(grid: any, current: Coord): (coord: Coord) => boolean {
  return (next: Coord) => {
    let pipeTo = grid[next[0]][next[1]];
    let pipeFrom = grid[current[0]][current[1]];
    let checkConnection = (dirs, to, from) => {
      return dirs.some(d => [d[0] + to[0], d[1] + to[1]].join(',') === from.join(','));
    };
    return (
      checkConnection(PIPES[pipeTo], next, current) &&
      (pipeFrom === 'S' || checkConnection(PIPES[pipeFrom], current, next))
    );
  };
}

function part1(input: string) {
  grid = input.trim().split('\n');
  let s: Coord = [0, 0];
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      if (grid[row][col] === 'S') {
        s = [row, col];
      }
    }
  }
  let visited = [] as Coord[];
  let steps = 0;
  while (true) {
    let coords: Coord[] = DIRS.map(([dr, dc]) => [s[0] + dr, s[1] + dc]);
    let pipeCoords = coords
      .filter(isValid)
      .filter(c => grid[c[0]][c[1]] !== '.')
      .filter(c => notVisited(visited, c))
      .filter(isConnected(grid, s));
    visited.push([...s]);
    if (pipeCoords.length === 0) break;
    let next = pipeCoords[0];
    s = [...next];
    steps += 1;
  }
  return Math.ceil(steps / 2);
}

function part2(input: string) {
  grid = input.trim().split('\n');
  let s: Coord = [0, 0];
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      if (grid[row][col] === 'S') {
        s = [row, col];
      }
    }
  }
  let visited = [] as Coord[];
  while (true) {
    let coords: Coord[] = DIRS.map(([dr, dc]) => [s[0] + dr, s[1] + dc]);
    let pipeCoords = coords
      .filter(isValid)
      .filter(c => grid[c[0]][c[1]] !== '.')
      .filter(c => notVisited(visited, c))
      .filter(isConnected(grid, s));
    visited.push([...s]);
    if (pipeCoords.length === 0) break;
    let next = pipeCoords[0];
    s = [...next];
  }

  // Scan rows
  let count = 0;
  for (let r = 0; r < grid.length; r++) {
    let isInside = false;
    for (let c = 0; c < grid[0].length; c++) {
      let isVisited = !notVisited(visited, [r, c]);
      if (isVisited && ['|', 'J', 'L'].includes(grid[r][c])) {
        isInside = !isInside;
      } else if (!isVisited && isInside) {
        count += 1;
      }
    }
  }
  return count;
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

// let testInput = `
// ..F7.
// .FJ|.
// SJ.L7
// |F--J
// LJ...
// `;
let testInput = `
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
