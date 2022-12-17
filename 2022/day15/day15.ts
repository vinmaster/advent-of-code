// deno-lint-ignore-file prefer-const

let useExample = false;
// useExample = true;

let input!: string;
if (!useExample) {
  input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);
} else {
  input = `
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3`;
}

console.log('part1:', part1(input));
console.log('part2:', part2(input));

function part1(input: string) {
  let lines = input.trim().split('\n');
  // let targetY = 10;
  // let grid = createGrid(lines);
  // let beacons = Object.entries(grid).filter(([_, v]) => v === 'S');
  // for (let [str, _] of beacons) {
  //   senseBeacon(grid, toCoords(str));
  // }
  // printGrid(grid);
  // return Object.entries(grid).filter(([s, v]) => toCoords(s).y === targetY && v === '#').length;

  // Solution
  let targetY = useExample ? 10 : 2_000_000;
  let distances: [Coord, number][] = [];
  let things = new Set<string>();
  for (let line of lines) {
    let [sx, sy, bx, by] = [...line.matchAll(/=(-?\d+)/g)].map(match => Number(match[1]));
    distances.push([{ x: sx, y: sy }, manhattanDistance({ x: sx, y: sy }, { x: bx, y: by })]);
    things.add(`${sx},${sy}`);
    things.add(`${bx},${by}`);
  }
  let count = 0;
  for (let x = -targetY * 10; x < targetY * 10; x++) {
    for (let [coord, dist] of distances) {
      let toCheck = { x, y: targetY };
      let isCovering = manhattanDistance(coord, toCheck) <= dist;
      if (isCovering && !things.has(toComma(toCheck))) {
        count += 1;
        break;
      }
    }
  }
  return count;
}

function part2(input: string) {
  let lines = input.trim().split('\n');

  // Attempt #1
  // let distances: [Coord, number][] = [];
  // let things = new Set<string>();
  // let min = { x: 0, y: 0 };
  // let max = { x: 0, y: 0 };
  // for (let line of lines) {
  //   let [sx, sy, bx, by] = [...line.matchAll(/=(-?\d+)/g)].map(match => Number(match[1]));
  //   distances.push([{ x: sx, y: sy }, manhattanDistance({ x: sx, y: sy }, { x: bx, y: by })]);
  //   things.add(`${sx},${sy}`);
  //   if (sx < min.x) min.x = sx;
  //   else if (sx > max.x) max.x = sx;
  //   if (sy < min.y) min.y = sy;
  //   else if (sy > max.y) max.y = sy;
  //   things.add(`${bx},${by}`);
  // }
  // for (let x = min.x; x <= max.x; x++) {
  //   let isStartCovering = false;
  //   for (let y = min.y; y <= max.y; y++) {
  //     let toCheck = { x, y };
  //     let isCovered = distances.some(([coord, dist]) => manhattanDistance(coord, toCheck) <= dist);
  //     if (isCovered) {
  //       isStartCovering = true;
  //     } else if (!isCovered && isStartCovering) {
  //       let next = { x, y: y + 1 };
  //       let actuallyDone = distances.some(
  //         ([coord, dist]) => manhattanDistance(coord, next) <= dist
  //       );
  //       if (actuallyDone) return toCheck.x * 4000000 + toCheck.y;
  //     }
  //   }
  // }

  // Attempt #2
  // let distances: [Coord, number][] = [];
  // for (let line of lines) {
  //   let [sx, sy, bx, by] = [...line.matchAll(/=(-?\d+)/g)].map(match => Number(match[1]));
  //   // add 1 to find intersection
  //   let dist = manhattanDistance({ x: sx, y: sy }, { x: bx, y: by }) + 1;
  //   distances.push([{ x: sx, y: sy }, dist]);
  // }
  // let min = 0;
  // let max = 4_000_000;
  // let visited: Record<string, number> = {};
  // for (let [coord, dist] of distances) {
  //   let cur = coord;
  //   cur.y -= dist;
  //   for (let d of Object.values(DIAG_DIRS)) {
  //     for (let i = 0; i < dist; i++) {
  //       cur = addCoords(cur, d);
  //       if (cur.x < min || cur.y < min || cur.x > max || cur.y > max) continue;
  //       visited[toComma(cur)] ??= 0;
  //       visited[toComma(cur)] += 1;
  //     }
  //   }
  // }
  // let entries = Object.entries(visited);
  // entries.sort(([_c1, a], [_c2, b]) => b - a);
  // let found = toCoords(entries[0][0]);
  // return found.x * 4000000 + found.y;

  // Attempt #3
  let distances: [Coord, number][] = [];
  for (let line of lines) {
    let [sx, sy, bx, by] = [...line.matchAll(/=(-?\d+)/g)].map(match => Number(match[1]));
    let dist = manhattanDistance({ x: sx, y: sy }, { x: bx, y: by });
    distances.push([{ x: sx, y: sy }, dist]);
  }
  // get the ranges that sensors cover
  distances.sort(([c1, _d1], [c2, _d2]) => c1.y - c2.y);
  let ranges: [number, number][] = [];
  for (let [c, d] of distances) {
    ranges.push([c.y, d]);
  }
  for (let i = 0; i < ranges.length - 1; ) {
    let [y1, d1] = ranges[i];
    let [y2, d2] = ranges[i + 1];
    if (y1 + d1 >= y2) {
      ranges.splice(i + 1, 1);
      continue;
    } else {
      i++;
    }
  }
  // only check those ranges
  let count = 0;
  let min = 0;
  let max = useExample ? 27 : 4_000_000;
  let found = [];

  for (let [start, range] of ranges) {
    for (let row = start; row <= start + range && row <= max; row++) {
      let xRanges: [number, number][] = [];
      for (let [coord, dist] of distances) {
        let res = intersectXRange(row, coord, dist);
        if (res) {
          xRanges.push(res);
        }
      }
      xRanges.sort(([x1, _d1], [x2, _d2]) => x1 - x2);
      for (let i = 0; i < xRanges.length - 1; ) {
        let [x1, d1] = xRanges[i];
        let [x2, d2] = xRanges[i + 1];
        if (x1 + d1 >= x2) {
          xRanges.splice(i + 1, 1);
          continue;
        } else if (x1 + d1 === x2 - 2 && d1 !== 0 && !covered(distances, x2 - 1, row)) {
          found.push({ x: x2 - 1, y: row });
          count++;
          i++;
        } else {
          i++;
        }
      }
    }
  }
  // console.log(found);
  // console.log(count);
  if (found.length > 1) throw new Error('False positive');
  return found[0].x * 4000000 + found[0].y;
}

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

const DIAG_DIRS = {
  DR: { y: 1, x: 1 },
  DL: { y: 1, x: -1 },
  UL: { y: -1, x: -1 },
  UR: { y: -1, x: 1 },
};

function toComma(coord: Coord) {
  return `${coord.x},${coord.y}`;
}

function toCoords(str: string) {
  let [x, y] = str.split(',').map(Number);
  return { x, y };
}

function addCoords(c: Coord, d: Coord): Coord {
  return { x: c.x + d.x, y: c.y + d.y };
}

function printGrid(grid: Record<string, string>) {
  let points = Object.keys(grid).map(toCoords);
  let xs = points.map(p => p.x);
  let ys = points.map(p => p.y);
  let [minX, maxX] = [Math.min(...xs), Math.max(...xs)];
  let [minY, maxY] = [Math.min(...ys), Math.max(...ys)];
  let yAxis = '';
  let ySpacing = 5;
  for (let x = minX; x <= maxX; x += ySpacing) {
    yAxis += x.toString().padStart(ySpacing);
  }
  console.log(yAxis);
  for (let y = minY; y <= maxY; y++) {
    let xAxis = y.toString().padStart(Math.max(minY.toString().length, maxY.toString().length));
    let line = `${xAxis} `;
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
    let [sx, sy, bx, by] = [...line.matchAll(/=(-?\d+)/g)].map(match => Number(match[1]));
    grid[`${sx},${sy}`] = 'S';
    grid[`${bx},${by}`] = 'B';
  }
  return grid;
}

function senseBeacon(grid: Record<string, string>, sensorCoord: Coord) {
  let queue: Coord[] = [sensorCoord];
  let visited = new Set<string>();
  let isFound = false;
  for (let i = 0; !isFound; i++) {
    let newQueue: Coord[] = [];
    for (let q of queue) {
      Object.values(DIRS).forEach(d => {
        let next = toComma(addCoords(q, d));
        if (grid[next] === 'B') {
          // found beacon
          isFound = true;
        }
        if (!visited.has(next) && !grid[next]) {
          visited.add(next);
          grid[next] = '#';
        }
        newQueue.push(toCoords(next));
      });
    }
    queue = newQueue;
  }
}

function manhattanDistance(c1: Coord, c2: Coord) {
  return Math.abs(c1.x - c2.x) + Math.abs(c1.y - c2.y);
}

function intersectXRange(row: number, coord: Coord, dist: number): [number, number] | null {
  let diff = Math.abs(row - coord.y);
  if (diff > dist) return null;
  let dx = dist - diff;
  // top or bottom only has 1 intersect
  // if (dx === 0) return [{ x: coord.x - dx, y: row }];
  // return [
  //   { x: coord.x - dx, y: row },
  //   { x: coord.x + dx, y: row },
  // ];
  return [coord.x - dx, dx * 2];
}

function covered(distances: [Coord, number][], x: number, y: number) {
  for (let [coord, dist] of distances) {
    if (manhattanDistance(coord, { x, y }) <= dist) {
      return true;
    }
  }
  return false;
}
