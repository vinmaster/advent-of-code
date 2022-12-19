// deno-lint-ignore-file prefer-const no-explicit-any

let DIRS: { x: number; y: number; z: number }[] = [];
for (let x = -1; x <= 1; x++) {
  for (let y = -1; y <= 1; y++) {
    for (let z = -1; z <= 1; z++) {
      if (Math.abs(x) + Math.abs(y) + Math.abs(z) === 1) {
        DIRS.push({ x, y, z });
      }
    }
  }
}

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// 2,2,2
// 1,2,2
// 3,2,2
// 2,1,2
// 2,3,2
// 2,2,1
// 2,2,3
// 2,2,4
// 2,2,6
// 1,2,5
// 3,2,5
// 2,1,5
// 2,3,5`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));

function part1(input: string) {
  let lines = input.trim().split('\n');
  let positions = {} as Record<PointStr, boolean>;
  let faces = 0;
  for (let line of lines as PointStr[]) {
    positions[line] = true;
  }

  for (let line of lines) {
    let dims = line.split(',').map(Number);
    for (let i = 0; i < dims.length; i++) {
      let copy1 = [...dims] as Point;
      let copy2 = [...dims] as Point;
      copy1.splice(i, 1, dims[i] + 1);
      copy2.splice(i, 1, dims[i] - 1);
      if (positions[toStr(copy1)]) faces++;
      if (positions[toStr(copy2)]) faces++;
    }
  }

  return Object.keys(positions).length * 6 - faces;
}

function part2(input: string) {
  let lines = input.trim().split('\n');
  let min = Infinity;
  let max = -Infinity;
  let points: Point[] = lines.map(l => {
    let point = l.split(',').map(Number) as Point;
    min = Math.min(min, ...point);
    max = Math.max(max, ...point);
    return point;
  });

  // let z = groupBy<Point>(points, p => p.at(2));
  // for (let plane of Object.values(z)) {
  //   let grid: string[][] = new Array(max).fill(0).map(() => new Array(max).fill('.'));
  //   for (let ps of plane) {
  //     grid[ps[1]][ps[0]] = '#';
  //   }
  //   print(grid);
  //   console.log('');
  //   for (let y = 0; y < grid.length; y++) {
  //     for (let x = 0; x < grid[y].length; x++) {}
  //   }
  // }

  let surfaceArea = 0;
  let visited = new Set<PointStr>();
  let queue: Point[] = [[0, 0, 0]];
  while (queue.length > 0) {
    let point = queue.shift()!;
    let pointStr = toStr(point);
    let [x, y, z] = point;
    if (visited.has(pointStr)) continue;
    if (points.findIndex(p => toStr(p) === pointStr) !== -1) continue;
    if (x < min - 1 || y < min - 1 || z < min - 1) continue;
    if (x > max + 1 || y > max + 1 || z > max + 1) continue;
    visited.add(pointStr);

    // check if moving towards direction is the surface of cube
    surfaceArea += DIRS.reduce((acc, d) => {
      if (points.findIndex(p => toStr(p) === toStr([x + d.x, y + d.y, z + d.z])) !== -1) {
        acc += 1;
      }
      return acc;
    }, 0);

    for (let d of DIRS) {
      queue.push([x + d.x, y + d.y, z + d.z]);
    }
  }

  return surfaceArea;
}

type Point = [number, number, number];
type PointStr = `${number},${number},${number}`;

function groupBy<T>(arr: T[], fn: (item: T) => any) {
  return arr.reduce((acc, item) => {
    const key = fn(item);
    acc[key] = acc[key] ?? [];
    acc[key].push(item);
    return acc;
  }, {} as Record<string, T[]>);
}

function print(grid: string[][]) {
  for (let y = 0; y < grid.length; y++) {
    let line = '';
    for (let x = 0; x < grid[y].length; x++) {
      line += grid[y][x];
    }
    console.log(line);
  }
}

function toStr(point: Point): PointStr {
  return point.toString() as PointStr;
}
