// deno-lint-ignore-file prefer-const

interface Coord {
  x: number;
  y: number;
}

const DIRS = {
  U: { y: -1, x: 0 },
  D: { y: 1, x: 0 },
  L: { y: 0, x: -1 },
  R: { y: 0, x: 1 },
};

const FALL_DIRS = {
  D: { y: 1, x: 0 },
  DL: { y: 1, x: -1 },
  DR: { y: 1, x: 1 },
};

function toComma(coord: Coord) {
  return `${coord.x},${coord.y}`;
}

function toCoords(str: string) {
  let [x, y] = str.split(',').map(Number);
  return { x, y };
}

function newCoord(c: Coord, d: Coord): Coord {
  return { x: c.x + d.x, y: c.y + d.y };
}

function printGrid(grid: Record<string, string>) {
  let points = Object.keys(grid).map(toCoords);
  let xs = points.map(p => p.x);
  let ys = points.map(p => p.y);
  let [minX, maxX] = [Math.min(...xs), Math.max(...xs)];
  let [minY, maxY] = [Math.min(...ys), Math.max(...ys)];
  for (let y = minY; y <= maxY; y++) {
    let line = '';
    for (let x = minX; x <= maxX; x++) {
      let value = grid[toComma({ x, y })] === undefined ? '.' : grid[toComma({ x, y })];
      line += value;
    }
    console.log(line);
  }
}

function createGrid(lines: string[]) {
  let grid = {} as Record<string, string>;
  for (let line of lines) {
    let m = line.match(/(\d+,\d+)/g)!;
    for (let i = 1; i < m.length; i++) {
      let from = m[i - 1];
      let to = m[i];
      let f = toCoords(from);
      let t = toCoords(to);
      let dx = Math.sign(t.x - f.x);
      let dy = Math.sign(t.y - f.y);
      grid[from] = '#';
      while (from !== to) {
        let next = newCoord(f, { x: dx, y: dy });
        from = toComma(next);
        f = toCoords(from);
        grid[from] = '#';
      }
    }
  }
  return grid;
}

function fallSand(grid: Record<string, string>) {
  while (true) {
    let cur: Coord = { x: 500, y: 0 };
    while (true) {
      let rest = true;
      for (let j = 0; j < Object.values(FALL_DIRS).length; j++) {
        let d = Object.values(FALL_DIRS)[j];
        let next = newCoord(cur, d);
        if (!grid[toComma(next)]) {
          rest = false;
          cur = next;
          break;
        }
      }
      if (rest) {
        grid[toComma(cur)] = 'o';
        break;
      }
      if (cur.y > 1000) {
        return;
      }
    }
  }
}

function fallSandWithFloor(grid: Record<string, string>, maxY: number) {
  while (true) {
    let cur: Coord = { x: 500, y: 0 };
    while (true) {
      let rest = true;
      for (let j = 0; j < Object.values(FALL_DIRS).length; j++) {
        let d = Object.values(FALL_DIRS)[j];
        let next = newCoord(cur, d);
        if (!grid[toComma(next)] && next.y < maxY) {
          rest = false;
          cur = next;
          break;
        }
      }
      if (rest) {
        grid[toComma(cur)] = 'o';
        if (cur.x === 500 && cur.y === 0) {
          return;
        }
        break;
      }
    }
  }
}

function part1(input: string) {
  let lines = input.trim().split('\n');
  let grid = createGrid(lines);

  grid['500,0'] = '+';

  fallSand(grid);
  // printGrid(grid);

  return Object.values(grid).filter(cell => cell === 'o').length;
}

function part2(input: string) {
  let lines = input.trim().split('\n');
  let grid = createGrid(lines);

  grid['500,0'] = '+';
  let maxY = Math.max(...Object.keys(grid).map(s => toCoords(s).y)) + 2;

  fallSandWithFloor(grid, maxY);
  // printGrid(grid);

  return Object.values(grid).filter(cell => cell === 'o').length;
}

function sleep(ms: number) {
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);
}

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// 498,4 -> 498,6 -> 496,6
// 503,4 -> 502,4 -> 502,9 -> 494,9`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
