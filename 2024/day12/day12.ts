type Grid = string[][];
type Coord = [number, number];
const DIR: Coord[] = [
  [-1, 0],
  [0, 1],
  [1, 0],
  [0, -1],
] as const;

function isValid(grid: string[][], coord: Coord): boolean {
  return coord[0] >= 0 && coord[0] < grid.length && coord[1] >= 0 && coord[1] < grid[0].length;
}

function addCoords(coord1: Coord, coord2: Coord): Coord {
  return [coord1[0] + coord2[0], coord1[1] + coord2[1]];
}

function getRegionCoords(grid: Grid, plantType: string, coord: Coord, regionCoords: string[]) {
  // Different plant type
  if (grid[coord[0]][coord[1]] !== plantType) return regionCoords;
  // Already visited
  if (regionCoords.includes(coord.toString())) return regionCoords;
  // Add to plant coords
  regionCoords.push(coord.toString());
  // Recurse
  let neighborCoords = DIR.map(d => addCoords(coord, d)).filter(coord => isValid(grid, coord));
  for (let nextCoord of neighborCoords) {
    getRegionCoords(grid, plantType, nextCoord, regionCoords);
  }
  return regionCoords;
}

function getPerimeter(coordStrs: string[]) {
  let perimeter = 0;
  // Add each wall that is not part of itself
  for (let coordStr of coordStrs) {
    let coord = coordStr.split(',').map(Number) as Coord;
    perimeter += DIR.map(d => addCoords(coord, d)).filter(
      coord => !coordStrs.includes(coord.toString())
    ).length;
  }
  return perimeter;
}

function getSides(grid: Grid, coordStrs: string[]) {
  let sides = 0;
  let wallsWithOrientation = [] as string[];

  // Had to for loop thru row/col because walls of coordStrs might not be next to each other
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      let coord = [row, col] as Coord;
      if (!coordStrs.includes(coord.toString())) continue;
      let wallList = DIR.map(
        (d, i) => [addCoords(coord, d), i % 2 === 0 ? 'h' : 'v'] as [Coord, 'h' | 'v']
      );
      for (let [wall, orientation] of wallList) {
        let reference = '';
        if (orientation === 'h') reference = coord[0].toString();
        else reference = coord[1].toString();
        // This is not a wall
        if (coordStrs.includes(wall.toString())) continue;

        // If wall is horizontal, get horizontal DIR, which are odd indexes
        let neighborWallCoord = DIR.filter((d, i) =>
          orientation === 'h' ? i % 2 !== 0 : i % 2 === 0
        )
          .map(d => addCoords(wall, d))
          .map(coord => `${reference} ${coord} ${orientation}`);
        // New wall coord with different orientation
        if (wallsWithOrientation.findIndex(x => neighborWallCoord.includes(x)) === -1) {
          sides += 1;
        }
        // Add each wall that is not part of itself
        wallsWithOrientation.push(`${reference} ${wall} ${orientation}`);
      }
    }
  }
  return sides;
}

export function part1(grid: Grid) {
  let regions = {} as Record<string, string[][]>;
  let price = 0;
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      let plantType = grid[row][col];

      regions[plantType] ??= [];
      if (!regions[plantType].some(coords => coords.includes([row, col].toString()))) {
        // Haven't visited
        let regionCoords = getRegionCoords(grid, plantType, [row, col], []);
        regions[plantType].push(regionCoords);
      }
    }
  }
  for (let plantType of Object.keys(regions)) {
    for (let region of regions[plantType]) {
      let area = region.length;
      let perimeter = getPerimeter(region);
      price += area * perimeter;
    }
  }
  return price;
}

export function part2(grid: Grid) {
  let regions = {} as Record<string, string[][]>;
  let price = 0;
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      let plantType = grid[row][col];
      regions[plantType] ??= [];
      if (!regions[plantType].some(coords => coords.includes([row, col].toString()))) {
        // Haven't visited
        let regionCoords = getRegionCoords(grid, plantType, [row, col], []);
        regions[plantType].push(regionCoords);
      }
    }
  }
  for (let plantType of Object.keys(regions)) {
    for (let region of regions[plantType]) {
      let area = region.length;
      let sides = getSides(grid, region);
      price += area * sides;
      // console.log(plantType, area, sides);
    }
  }
  return price;
}

function parseInput(inputString: string): Grid {
  return inputString
    .trim()
    .split('\n')
    .map(line => line.split(''));
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
AAAA
BBCD
BBCC
EEEC
`;

  //   testInput = `
  // OOOOO
  // OXOXO
  // OOOOO
  // OXOXO
  // OOOOO
  //   `;

  testInput = `
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE`;

  //   testInput = `
  // EEEEE
  // EXXXX
  // EEEEE
  // EXXXX
  // EEEEE`;

  if (!useRealInput) inputString = testInput;

  let input = parseInput(inputString);
  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// await main(false);
await main();
