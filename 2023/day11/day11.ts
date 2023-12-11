type Coord = [number, number];

function manhattanDistance(c1: Coord, c2: Coord) {
  return Math.abs(c1[0] - c2[0]) + Math.abs(c1[1] - c2[1]);
}

function manhattanDistanceAcrossSpace(
  c1: Coord,
  c2: Coord,
  { emptyRows, emptyCols }: { emptyRows: number[]; emptyCols: number[] },
  emptySpaceDistance: number
) {
  let deltaR = Math.sign(c2[0] - c1[0]);
  let deltaC = Math.sign(c2[1] - c1[1]);
  let distance = 0;
  if (deltaR !== 0) {
    for (let r = c1[0]; r !== c2[0]; r += deltaR) {
      if (emptyRows.includes(r)) {
        distance += emptySpaceDistance;
      } else {
        distance += 1;
      }
    }
  }
  if (deltaC !== 0) {
    for (let c = c1[1]; c !== c2[1]; c += deltaC) {
      if (emptyCols.includes(c)) {
        distance += emptySpaceDistance;
      } else {
        distance += 1;
      }
    }
  }
  return distance;
}

function part1(input: string) {
  let grid = input.trim().split('\n');
  let emptyRows: number[] = grid
    .map((row, index) => [row.replaceAll(/\./g, '').length, index])
    .filter(([count, index]) => count === 0)
    .map(([_, index]) => index);
  let emptyCols: number[] = [];
  for (let col = 0; col < grid[0].length; col++) {
    let colLine = '';
    for (let row of grid) {
      colLine += row[col];
    }
    if (colLine.replaceAll(/\./g, '').length === 0) {
      emptyCols.push(col);
    }
  }
  // Expand row
  let emptyRowStr = '.'.repeat(grid[0].length);
  for (let index of emptyRows.reverse()) {
    grid.splice(index, 0, emptyRowStr);
  }
  // Expand col
  for (let index of emptyCols.reverse()) {
    for (let row = 0; row < grid.length; row++) {
      grid[row] = grid[row].slice(0, index) + '.' + grid[row].slice(index, grid[row].length);
    }
  }
  // Print
  // for (let row of grid) {
  //   console.log(row);
  // }
  let galaxies: Coord[] = [];
  for (let r = 0; r < grid.length; r++) {
    for (let c = 0; c < grid[0].length; c++) {
      if (grid[r][c] === '#') {
        galaxies.push([r, c]);
      }
    }
  }
  let sum = 0;
  for (let i = 0; i < galaxies.length; i++) {
    for (let j = i; j < galaxies.length; j++) {
      if (i !== j) {
        // sum += manhattanDistanceAcrossSpace(galaxies[i], galaxies[j], { emptyRows, emptyCols }, 1);
        sum += manhattanDistance(galaxies[i], galaxies[j]);
      }
    }
  }
  return sum;
}

function part2(input: string) {
  let grid = input.trim().split('\n');
  let emptyRows: number[] = grid
    .map((row, index) => [row.replaceAll(/\./g, '').length, index])
    .filter(([count, index]) => count === 0)
    .map(([_, index]) => index);
  let emptyCols: number[] = [];
  for (let col = 0; col < grid[0].length; col++) {
    let colLine = '';
    for (let row of grid) {
      colLine += row[col];
    }
    if (colLine.replaceAll(/\./g, '').length === 0) {
      emptyCols.push(col);
    }
  }

  // Print
  // for (let row of grid) {
  //   console.log(row);
  // }
  let galaxies: Coord[] = [];
  for (let r = 0; r < grid.length; r++) {
    for (let c = 0; c < grid[0].length; c++) {
      if (grid[r][c] === '#') {
        galaxies.push([r, c]);
      }
    }
  }
  let sum = 0;
  for (let i = 0; i < galaxies.length; i++) {
    for (let j = i; j < galaxies.length; j++) {
      if (i !== j) {
        sum += manhattanDistanceAcrossSpace(
          galaxies[i],
          galaxies[j],
          { emptyRows, emptyCols },
          1_000_000
        );
      }
    }
  }
  return sum;
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
