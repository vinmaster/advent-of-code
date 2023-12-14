function transpose(array: any[]) {
  return array[0].map((_, colIndex) => array.map(row => row[colIndex]));
}

function rotate90(array: any[][]) {
  return array[0].map((val, index) => array.map(row => row[index]).reverse());
}

function rotate180(array: any[][]) {
  return array.reverse().map(row => row.reverse());
}

function rotate270(array: any[][]) {
  return array[0].map((val, index) => array.map(row => row[row.length - 1 - index]));
}

function allIndexes(str: string, substr: string) {
  let indexes = [] as number[];
  let i = str.indexOf(substr);
  while (i !== -1) {
    indexes.push(i);
    i = str.indexOf(substr, i + 1);
  }
  return indexes;
}

function tiltWest(grid: string[][]) {
  let newGrid: string[][] = [];
  for (let row of grid) {
    let groups: string[] = [];
    // Find groups of #
    let indexes = allIndexes(row.join(''), '#');
    let i = 0;
    for (let index of indexes) {
      let slice = row.slice(i, index).join('');
      if (slice !== '') groups.push(slice);
      i = index;
    }
    // Get substring of # groups
    groups.push(row.slice(i, row.length).join(''));
    let newLine = '';
    // Create new string based on groups
    for (let g of groups) {
      let os = g.split('O').length - 1;
      let ps = g.split('#').length - 1;
      newLine += ('#'.repeat(ps) + 'O'.repeat(os)).padEnd(g.length, '.');
    }
    newGrid.push(newLine.split(''));
  }
  return newGrid;
}

function cycle(grid: string[][]) {
  // Tilt north
  grid = tiltWest(rotate270(grid));
  // Tilt west
  grid = tiltWest(rotate90(grid));
  // Tilt south
  grid = tiltWest(rotate90(grid));
  // Tilt east
  grid = tiltWest(rotate90(grid));
  // Fix rotation
  grid = rotate180(grid);
  return grid;
}

function totalLoad(grid: string[][]) {
  let r = grid.map((row, i) => {
    return row.reduce((sum, cell) => {
      if (cell === 'O') return sum + grid.length - i;
      return sum;
    }, 0);
  });
  return r.reduce((a, b) => a + b);
}

function part1(input: string) {
  let grid = input
    .trim()
    .split('\n')
    .map(line => line.split(''));
  // Rotate -90
  // grid = transpose(grid);
  // let newGrid = transpose(tiltWest(grid));
  let newGrid = rotate90(tiltWest(rotate270(grid)));
  return totalLoad(newGrid);
}

function part2(input: string) {
  let grid = input
    .trim()
    .split('\n')
    .map(line => line.split(''));
  // grid.forEach(row => console.log(row.join('')));

  let map = new Map();
  let found: number[] = [];
  for (let i = 0; i < 2_000; i++) {
    grid = cycle(grid);
    let gridStr = grid.map(row => row.join('')).join('');
    if (map.has(gridStr)) {
      let indexes = map.get(gridStr);
      if (indexes.length === 2) {
        found = indexes;
        break;
      }
      map.set(gridStr, [...indexes, i + 1]);
    } else {
      map.set(gridStr, [i + 1]);
    }
  }

  let offset = found[0];
  let cycleLength = found[1] - found[0];
  let index = (1_000_000_000 - offset) % cycleLength;
  for (let i = 0; i < 2_000; i++) {
    grid = cycle(grid);
    if (i === index - 1) return totalLoad(grid);
  }
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
