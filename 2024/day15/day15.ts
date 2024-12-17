type Input = { grid: Grid; moves: Direction[] };
type Coord = [number, number];
const DIR = ['^', '>', 'v', '<'] as const;
type Direction = (typeof DIR)[number];
const DIR_DELTA: Record<Direction, Coord> = {
  '^': [-1, 0],
  '>': [0, 1],
  v: [1, 0],
  '<': [0, -1],
};

class Grid {
  source: string;
  rows: number;
  cols: number;
  constructor(source: string) {
    this.source = source;
    this.cols = source.indexOf('\n');
    if (this.cols === -1) {
      this.cols = source.length;
      this.rows = 1;
    } else {
      const sourceLength = source.length + 1; // Last \n was trimmed
      this.rows = sourceLength / (this.cols + 1);
    }
  }
  isValid(coord: Coord): boolean {
    return coord[0] >= 0 && coord[1] >= 0 && coord[0] < this.rows && coord[1] < this.cols;
  }
  get(coord: Coord) {
    return this.source[this.toIndex(coord)];
  }
  set(coord: Coord, value: string) {
    let index = this.toIndex(coord);
    this.source = this.source.substring(0, index) + value + this.source.substring(index + 1);
  }
  toIndex(coord: Coord) {
    return (this.cols + 1) * coord[0] + coord[1];
  }
  toCoord(index: number): Coord {
    const row = Math.floor(index / (this.cols + 1));
    const col = index % (this.cols + 1);
    return [row, col];
  }
}

function addCoord(a, b): Coord {
  return [a[0] + b[0], a[1] + b[1]];
}

function moveDir(coord: Coord, dir: Direction, steps = 1): Coord {
  for (let i = 0; i < steps; i++) {
    coord = addCoord(coord, DIR_DELTA[dir]);
  }
  return coord;
}

function copyCoordTo(source, target) {
  target[0] = source[0];
  target[1] = source[1];
}

function isBox(grid: Grid, coord: Coord): boolean {
  return grid.get(coord) === '[' || grid.get(coord) === ']';
}

function pushBox(grid: Grid, coord: Coord, dir: Direction): boolean {
  let original = [...coord] as Coord;
  let current = 'O';
  do {
    coord = addCoord(coord, DIR_DELTA[dir]);
    current = grid.get(coord);
  } while (current === 'O');
  if (current === '.') {
    grid.set(original, '.');
    grid.set(coord, 'O');
    return true;
  }
  return false;
}

function moveRobot(grid: Grid, current: Coord, move: Direction) {
  let nextCoord = addCoord(current, DIR_DELTA[move]);
  let next = grid.get(nextCoord);
  if (next === '.') {
    grid.set(current, '.');
    grid.set(nextCoord, '@');
    copyCoordTo(nextCoord, current);
  } else if (next === 'O') {
    let wasPushed = pushBox(grid, nextCoord, move);
    if (wasPushed) {
      grid.set(current, '.');
      grid.set(nextCoord, '@');
      copyCoordTo(nextCoord, current);
    }
  }
}

function pushBox2(grid: Grid, coord: Coord, dir: Direction): boolean {
  let current = grid.get(coord);
  let dirType: 'vertical' | 'horizontal' = DIR.indexOf(dir) % 2 === 0 ? 'vertical' : 'horizontal';
  if (dirType === 'horizontal') {
    // Recurse 2 at a time because of box horizontal size
    let nextCoord = moveDir(coord, dir, 2);
    let nextValue = grid.get(nextCoord);
    if (nextValue === '.') {
      let boxPart2 = moveDir(coord, dir);
      let boxPart1 = coord;
      grid.set(nextCoord, grid.get(boxPart2));
      grid.set(boxPart2, grid.get(boxPart1));
      return true;
    } else if (isBox(grid, nextCoord)) {
      let wasPushed = pushBox2(grid, nextCoord, dir);
      if (wasPushed) {
        let boxPart2 = moveDir(coord, dir);
        let boxPart1 = coord;
        grid.set(nextCoord, grid.get(boxPart2));
        grid.set(boxPart2, grid.get(boxPart1));
        return true;
      }
    }
    return false;
  } else {
    if (grid.get(coord) === '.') {
      return true;
    } else if (isBox(grid, coord)) {
      // Other side of the box
      let otherCoord = current === ']' ? moveDir(coord, '<') : moveDir(coord, '>');
      let nextCoord1 = moveDir(coord, dir);
      let nextCoord2 = moveDir(otherCoord, dir);
      if (pushBox2(grid, nextCoord1, dir) && pushBox2(grid, nextCoord2, dir)) {
        // Successfully able to push the box
        grid.set(nextCoord1, grid.get(coord));
        grid.set(nextCoord2, grid.get(otherCoord));
        grid.set(coord, '.');
        grid.set(otherCoord, '.');
        return true;
      }
    }
    return false;
  }
}

function moveRobot2(grid: Grid, current: Coord, dir: Direction) {
  let nextCoord = moveDir(current, dir);
  let next = grid.get(nextCoord);
  if (next === '.') {
    grid.set(current, '.');
    grid.set(nextCoord, '@');
    copyCoordTo(nextCoord, current);
  } else if (isBox(grid, nextCoord)) {
    let gridCopy = new Grid(grid.source);
    let wasPushed = pushBox2(gridCopy, nextCoord, dir);
    if (wasPushed) {
      grid.source = gridCopy.source;
      grid.set(current, '.');
      grid.set(nextCoord, '@');
      copyCoordTo(nextCoord, current);
    }
  }
}

export function part1(input: Input) {
  let { grid, moves } = input;
  let robot = grid.toCoord(grid.source.indexOf('@'));
  let sum = 0;
  for (let move of moves) {
    moveRobot(grid, robot, move);
  }
  for (let row = 0; row < grid.rows; row++) {
    for (let col = 0; col < grid.cols; col++) {
      if (grid.get([row, col]) === 'O') {
        sum += 100 * row + col;
      }
    }
  }
  return sum;
}

export function part2(input: Input) {
  let { grid, moves } = input;
  let sum = 0;
  // Expand grid
  let newSource = grid.source
    .replaceAll('#', '##')
    .replaceAll('O', '[]')
    .replaceAll('.', '..')
    .replaceAll('@', '@.');
  grid = new Grid(newSource);
  let robot = grid.toCoord(grid.source.indexOf('@.'));
  for (let move of moves) {
    moveRobot2(grid, robot, move);
    // console.log('robot', robot, move);
    // console.log(grid.source);
  }
  for (let row = 0; row < grid.rows; row++) {
    for (let col = 0; col < grid.cols; col++) {
      if (grid.get([row, col]) === '[') {
        sum += 100 * row + col;
      }
    }
  }
  return sum;
}

function parseInput(inputString: string): Input {
  let [map, movesStr] = inputString.trim().split('\n\n');

  return { grid: new Grid(map), moves: movesStr.split('').filter(m => m !== '\n') as Direction[] };
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
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
`;

  testInput = `
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^`;

  //   testInput = `
  // #######
  // #...#.#
  // #.....#
  // #..OO@#
  // #..O..#
  // #.....#
  // #######

  // <vv<<^^<<^^`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
