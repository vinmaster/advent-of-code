const DIR = ['UP', 'DOWN', 'LEFT', 'RIGHT'] as const;
type Direction = (typeof DIR)[number];
type Coord = [number, number];
type Beam = [Coord, Direction];
const DIR_DELTA: Record<Direction, Coord> = {
  UP: [-1, 0],
  DOWN: [1, 0],
  LEFT: [0, -1],
  RIGHT: [0, 1],
};
const CELL_TYPES = ['.', '/', '\\', '|', '-'] as const;
const MOVEMENTS: Record<(typeof CELL_TYPES)[number], Record<Direction, Direction[]>> = {
  '.': { UP: ['UP'], DOWN: ['DOWN'], LEFT: ['LEFT'], RIGHT: ['RIGHT'] },
  '/': { UP: ['RIGHT'], DOWN: ['LEFT'], LEFT: ['DOWN'], RIGHT: ['UP'] },
  '\\': { UP: ['LEFT'], DOWN: ['RIGHT'], LEFT: ['UP'], RIGHT: ['DOWN'] },
  '|': { UP: ['UP'], DOWN: ['DOWN'], LEFT: ['UP', 'DOWN'], RIGHT: ['UP', 'DOWN'] },
  '-': { UP: ['LEFT', 'RIGHT'], DOWN: ['LEFT', 'RIGHT'], LEFT: ['LEFT'], RIGHT: ['RIGHT'] },
};

function validCoordGenerator(grid: string[][]) {
  return (coord: Coord): boolean =>
    coord[0] >= 0 && coord[0] < grid.length && coord[1] >= 0 && coord[1] < grid[0].length;
}

function moveCoord(coord: Coord, dir: Direction): Coord {
  let [dr, dc] = DIR_DELTA[dir];
  return [coord[0] + dr, coord[1] + dc];
}

function printGrid(grid) {
  let [minR, maxR] = [0, grid.length];
  let [minC, maxC] = [0, grid[0].length];
  for (let r = minR; r < maxR; r++) {
    let line = '';
    for (let c = minC; c < maxC; c++) {
      let value = grid[r][c] !== '.' ? '#' : '.';
      line += value;
    }
    console.log(line);
  }
}

function part1(input: string) {
  let grid = input
    .trim()
    .split('\n')
    .map(row => row.split(''));
  let beams: Beam[] = [[[0, 0], 'RIGHT']];
  let isValid = validCoordGenerator(grid);
  let visited: Record<string, Direction[]> = { '0,0': ['RIGHT'] };
  // grid.forEach(row => console.log(row));

  while (beams.length > 0) {
    beams = beams.flatMap(([coord, dir]) => {
      let cell = grid[coord[0]][coord[1]];
      let movements: Direction[] = MOVEMENTS[cell][dir];
      return movements
        .map(newDir => [moveCoord(coord, newDir), newDir] as Beam)
        .filter(([coord, _]) => isValid(coord))
        .filter(
          ([coord, dir]) => !(visited[coord.toString()] && visited[coord.toString()].includes(dir))
        );
    });
    beams.forEach(([coord, dir]) => {
      visited[coord.toString()] ??= [];
      visited[coord.toString()] = [...new Set([...visited[coord.toString()], dir])];
    });
  }
  // printGrid(grid);
  return Object.keys(visited).length;
}

function part2(input: string) {
  let grid = input
    .trim()
    .split('\n')
    .map(row => row.split(''));
  let isValid = validCoordGenerator(grid);
  let max = 0;

  let runBeams = (r, c, initDir: Direction) => {
    let beams: Beam[] = [[[r, c], initDir]];
    let visited: Record<string, Direction[]> = {};
    visited[`${beams[0][0][0]},${beams[0][0][1]}`] = [beams[0][1]];
    // grid.forEach(row => console.log(row));
    while (beams.length > 0) {
      beams = beams.flatMap(([coord, dir]) => {
        let cell = grid[coord[0]][coord[1]];
        let movements: Direction[] = MOVEMENTS[cell][dir];
        return movements
          .map(newDir => [moveCoord(coord, newDir), newDir] as Beam)
          .filter(([coord, _]) => isValid(coord))
          .filter(
            ([coord, dir]) =>
              !(visited[coord.toString()] && visited[coord.toString()].includes(dir))
          );
      });
      beams.forEach(([coord, dir]) => {
        visited[coord.toString()] ??= [];
        visited[coord.toString()] = [...new Set([...visited[coord.toString()], dir])];
      });
    }
    // printGrid(grid);
    return Object.keys(visited).length;
  };
  for (let c = 0; c < grid[0].length; c++) {
    let energized = runBeams(0, c, 'DOWN');
    if (max < energized) {
      max = energized;
    }
  }
  for (let c = 0; c < grid[0].length; c++) {
    let energized = runBeams(grid.length - 1, c, 'UP');
    if (max < energized) {
      max = energized;
    }
  }
  for (let r = 0; r < grid.length; r++) {
    let energized = runBeams(r, 0, 'RIGHT');
    let energized2 = runBeams(r, grid[0].length - 1, 'LEFT');
    if (max < energized) {
      max = energized;
    }
    if (max < energized2) {
      max = energized2;
    }
  }
  return max;
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
.|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
