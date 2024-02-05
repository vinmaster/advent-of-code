function printGrid(coords: number[][]) {
  let maxY = Math.max(...coords.map(c => c[1]));
  let maxX = Math.max(...coords.map(c => c[0]));
  let coordStrs = coords.map(c => c.toString());
  for (let y = 0; y <= maxY; y++) {
    let line = '';
    for (let x = 0; x <= maxX; x++) {
      if (coordStrs.includes(`${x},${y}`)) {
        line += '#';
      } else {
        line += ' ';
      }
    }
    console.log(line);
  }
}

function foldCoords(coords, fold) {
  let [axis, n] = fold;
  for (let i = 0; i < coords.length; i++) {
    let [x, y] = coords[i];
    if (axis === 'y' && y > n) {
      coords[i][1] = n - (y - n);
    } else if (axis === 'x' && x >= n) {
      coords[i][0] = n - (x - n);
    }
  }
  return coords;
}

function part1(input: string) {
  let [linesInput, foldsInput] = input.trim().split('\n\n');
  let lines = linesInput.split('\n');
  let folds: [string, number][] = foldsInput.split('\n').map(fold => {
    let [, axis, num] = fold.match(/.*(x|y)=(.*)/)!;
    return [axis, +num];
  });
  let coords = lines.map(line => line.split(',').map(Number));
  coords = foldCoords(coords, folds[0]);
  let unique = [...new Set(coords.map(c => c.toString()))];
  return unique.length;
}

function part2(input: string) {
  let [linesInput, foldsInput] = input.trim().split('\n\n');
  let lines = linesInput.split('\n');
  let folds: [string, number][] = foldsInput.split('\n').map(fold => {
    let [, axis, num] = fold.match(/.*(x|y)=(.*)/)!;
    return [axis, +num];
  });
  let coords = lines.map(line => line.split(',').map(Number));
  coords = folds.reduce(foldCoords, coords);
  printGrid(coords);
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
