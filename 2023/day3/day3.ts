let MAX_ROW = 0;
let MAX_COL = 0;

function part1(input: string) {
  const lines = input.trim().split('\n');
  let sum = 0;
  MAX_ROW = lines.length;
  MAX_COL = lines[0].length;
  for (let row = 0; row < MAX_ROW; row++) {
    let line = lines[row];
    let matches = [...line.matchAll(/(\d+)/g)].map(m => m[0]);
    // Make it unique
    matches = [...new Set(matches)];
    if (matches.length > 0) {
      matches.forEach(m => {
        // Just in case same number appears multiple times in line
        let indexes = allIndexes(line, m);
        // Make sure this number is not substring of another number
        indexes = indexes.filter(
          i => isNaN(Number(line[i - 1])) && isNaN(Number(line[i + m.length]))
        );
        let neighborsCoordsList = indexes.map(i => getNeighborCoords(row, i, m.length));
        for (let neighborsCoords of neighborsCoordsList) {
          let isPartNumber = neighborsCoords.some(coord => {
            return lines[coord[0]][coord[1]] !== '.' && lines[coord[0]][coord[1]].match(/\D/);
          });
          if (isPartNumber) {
            sum += Number(m);
          }
        }
      });
    }
  }
  return sum;
}

function part2(input: string) {
  const lines = input.trim().split('\n');
  MAX_ROW = lines.length;
  MAX_COL = lines[0].length;
  let gearRatios = [] as { coord: string; number: number }[];
  for (let row = 0; row < MAX_ROW; row++) {
    let line = lines[row];
    let matches = [...line.matchAll(/(\d+)/g)].map(m => m[0]);
    // Make it unique
    matches = [...new Set(matches)];
    if (matches.length > 0) {
      matches.forEach(m => {
        // Just in case same number appears multiple times in line
        let indexes = allIndexes(line, m);
        // Make sure this number is not substring of another number
        indexes = indexes.filter(
          i => isNaN(Number(line[i - 1])) && isNaN(Number(line[i + m.length]))
        );
        let neighborsCoordsList = indexes.map(i => getNeighborCoords(row, i, m.length));
        for (let neighborsCoords of neighborsCoordsList) {
          for (let coord of neighborsCoords) {
            if (lines[coord[0]][coord[1]] === '*') {
              gearRatios.push({ coord: `${coord[0]},${coord[1]}`, number: Number(m) });
            }
          }
        }
      });
    }
  }
  return Object.values(groupBy(gearRatios, 'coord'))
    .filter(ratios => ratios.length === 2)
    .map(ratio => ratio[0].number * ratio[1].number)
    .reduce((a, b) => a + b);
}

function getNeighborCoords(row: number, col: number, length: number) {
  let coords = [] as [number, number][];
  for (let c = col - 1; c < col + length + 1; c++) {
    coords.push([row - 1, c]);
    coords.push([row + 1, c]);
  }
  coords.push([row, col - 1]);
  coords.push([row, col + length]);
  return coords.filter(isValid);
}

function isValid([row, col]: [row: number, col: number]) {
  return row >= 0 && row < MAX_ROW && col >= 0 && col < MAX_COL;
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

function groupBy(xs, key): Record<string, any[]> {
  return xs.reduce(function (rv, x) {
    (rv[x[key]] = rv[x[key]] || []).push(x);
    return rv;
  }, {});
}

let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

// input = `
// 467..114..
// ...*......
// ..35..633.
// ......#...
// 617*......
// .....+.58.
// ..592.....
// ......755.
// ...$.*....
// .664.598..
// `;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
