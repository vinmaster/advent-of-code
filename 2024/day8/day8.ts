type Coord = [number, number];

function isValid(grid: string[][], coord: Coord): boolean {
  return coord[0] >= 0 && coord[0] < grid.length && coord[1] >= 0 && coord[1] < grid[0].length;
}

export function part1(input: string) {
  input = input.trim();
  let grid = input.split('\n').map(line => line.split(''));
  let isAntenna = char => !['\n', '#', '.'].includes(char);
  let antennaCoordsMap: Record<string, Coord[]> = {};
  let antinodeCoords: string[] = [];

  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      let value = grid[row][col];
      if (isAntenna(value)) {
        if (!antennaCoordsMap[value]) antennaCoordsMap[value] = [];
        antennaCoordsMap[value].push([row, col]);
        for (let coord of antennaCoordsMap[value]) {
          if (coord.toString() === [row, col].toString()) continue;
          let [dr, dc] = [row - coord[0], col - coord[1]];
          let fromCurrent = [row + dr, col + dc] as Coord;
          if (isValid(grid, fromCurrent) && !antinodeCoords.includes(fromCurrent.toString())) {
            antinodeCoords.push(fromCurrent.toString());
          }
          let fromCoord = [coord[0] - dr, coord[1] - dc] as Coord;
          if (isValid(grid, fromCoord) && !antinodeCoords.includes(fromCoord.toString())) {
            antinodeCoords.push(fromCoord.toString());
          }
        }
      }
    }
  }
  return antinodeCoords.length;
}

export function part2(input: string) {
  input = input.trim();
  let grid = input.split('\n').map(line => line.split(''));
  let isAntenna = char => !['\n', '#', '.'].includes(char);
  let antennaCoordsMap: Record<string, Coord[]> = {};
  let antinodeCoords: string[] = [];

  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      let value = grid[row][col];
      if (isAntenna(value)) {
        if (!antennaCoordsMap[value]) antennaCoordsMap[value] = [];
        antennaCoordsMap[value].push([row, col]);
        for (let coord of antennaCoordsMap[value]) {
          if (coord.toString() === [row, col].toString()) continue;
          let [dr, dc] = [row - coord[0], col - coord[1]];
          let fromCurrent = [row, col] as Coord;
          while (true) {
            if (!antinodeCoords.includes(fromCurrent.toString())) {
              antinodeCoords.push(fromCurrent.toString());
            }
            fromCurrent = [fromCurrent[0] + dr, fromCurrent[1] + dc] as Coord;
            if (!isValid(grid, fromCurrent)) break;
          }
          let fromCoord = coord as Coord;
          while (true) {
            if (!antinodeCoords.includes(fromCoord.toString())) {
              antinodeCoords.push(fromCoord.toString());
            }
            fromCoord = [fromCoord[0] - dr, fromCoord[1] - dc] as Coord;
            if (!isValid(grid, fromCoord)) break;
          }
        }
      }
    }
  }
  return antinodeCoords.length;
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
......#....#
...#....0...
....#0....#.
..#....0....
....0....#..
.#....A.....
...#........
#......#....
........A...
.........A..
..........#.
..........#.
`;
  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(inputString));
  console.log('part2:', part2(inputString));
}

// await main(false);
await main();
