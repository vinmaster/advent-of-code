const DIR = ['UP', 'DOWN', 'LEFT', 'RIGHT'] as const;
type Direction = (typeof DIR)[number];
type Coord = [number, number];
// type CoordStr = `${number},${number}`;
type CoordStr = string;
const DIR_DELTA: Record<Direction, Coord> = {
  UP: [0, -1],
  DOWN: [0, 1],
  LEFT: [-1, 0],
  RIGHT: [1, 0],
};
type GridMap = Record<CoordStr, string>;

function moveCoord(coord: Coord, dir: Direction, distance: number = 1): Coord {
  let [dr, dc] = DIR_DELTA[dir];
  return [coord[0] + dr * distance, coord[1] + dc * distance];
}

function toCoordStr(coord: Coord) {
  return `${coord[0]},${coord[1]}`;
}

function toCoords(str: CoordStr): Coord {
  let [x, y] = str.split(',').map(Number);
  return [x, y];
}

// Shoelace formula
function getPolygonArea(coords: Coord[]) {
  let sum = 0;
  for (let i = 0; i < coords.length; i++) {
    let j = (i + 1) % coords.length;
    sum += coords[i][0] * coords[j][1] - coords[i][1] * coords[j][0];
  }
  return Math.abs(sum) * 0.5;
}

/*
Credit: Reddit
Pick's theorem problem. The instructions in the input essentially describe 
a polygon with integer vertices, and the number of cubic meters of lava 
is the number of interior points of the polygon plus the number of boundary points. 
You can count the number of boundary points by just adding up all the distances 
in the instructions, and you can figure out the area by applying shoelace formula 
to the vertices of the polygon. Once you have those two pieces of information, 
apply Pick's theorem to get the number of interior points, and then add the 
number of interior points to the number of boundary points to get the answer.
*/

/*
Pick's theorem
  A = i + (b / 2) - 1
  A = area
  i = interior points
  b = boundary points (perimeter)

We want i + b
i + b = A + (b / 2) + 1
*/
function part1(input: string) {
  let lines = input.trim().split('\n');
  let current: Coord = [0, 0];
  let grid: GridMap = {};
  grid[toCoordStr(current)] = '#';
  let coords: Coord[] = [current];
  let perimeter = 0;
  for (let line of lines) {
    let [dirShort, stepsStr, _] = /^(\w) (\d+) \(\#(.+)\)$/.exec(line)!.slice(1);
    let steps = Number(stepsStr);
    let dir = DIR.filter(d => d.startsWith(dirShort))[0];
    current = moveCoord(current, dir, steps);
    coords.push(current);
    perimeter += steps;
  }
  let area = getPolygonArea(coords);
  return area + perimeter / 2 + 1;
}

function part2(input: string) {
  let lines = input.trim().split('\n');
  let current: Coord = [0, 0];
  let grid: GridMap = {};
  grid[toCoordStr(current)] = '#';
  let coords: Coord[] = [current];
  let perimeter = 0;
  let dirMapping: Direction[] = ['RIGHT', 'DOWN', 'LEFT', 'UP'];
  for (let line of lines) {
    let [hex, dirStr] = /^\w \d+ \(\#(.{5})(.)\)$/.exec(line)!.slice(1);
    let steps = parseInt(hex, 16);
    let dir = dirMapping[dirStr];
    current = moveCoord(current, dir, steps);
    coords.push(current);
    perimeter += steps;
  }
  let area = getPolygonArea(coords);
  return area + perimeter / 2 + 1;
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
