import util from 'util';

const getIsWithinRange = (min, max) => number => number >= min && number <= max;

function* enumerate<T>(xs: Iterable<T>): IterableIterator<[number, T]> {
  let i = 0;
  for (const x of xs) {
    yield [i++, x];
  }
}

// ax + by = c
class Hailstone {
  sx: number;
  sy: number;
  sz: number;
  vx: number;
  vy: number;
  vz: number;
  a: number;
  b: number;
  c: number;
  constructor(sx, sy, sz, vx, vy, vz) {
    this.sx = sx;
    this.sy = sy;
    this.sz = sz;
    this.vx = vx;
    this.vy = vy;
    this.vz = vz;

    this.a = vy;
    this.b = -vx;
    this.c = vy * sx - vx * sy;
  }
  get [Symbol.toStringTag]() {
    return `${this}`;
  }
  [util.inspect.custom]() {
    return `${this}`;
  }
  toString() {
    return `Hailstone{a=${this.a}, b=${this.b}, c=${this.c}}`;
  }
}

function getRockVelocity(velocities: Record<number, number[]>): number {
  let possibleV: number[] = [];
  for (let x = -1000; x <= 1000; x++) {
    possibleV.push(x);
  }
  Object.keys(velocities).forEach(velocity => {
    const vel = parseInt(velocity, 10);
    if (velocities[vel].length < 2) {
      return;
    }
    let newPossibleV: number[] = [];
    possibleV.forEach(possible => {
      if ((velocities[vel][0] - velocities[vel][1]) % (possible - vel) === 0) {
        newPossibleV.push(possible);
      }
    });
    possibleV = newPossibleV;
  });

  return possibleV[0];
}

// Credit: https://github.com/hyper-neutrino/advent-of-code/blob/main/2023/day24p1.py
function part1(input: string) {
  let lines = input.trim().split('\n');
  let hailstones = lines.map(
    line => new Hailstone(...line.replace('@', ',').split(',').map(Number))
  );
  let limits = [200000000000000, 400000000000000];
  let isWithinRange = getIsWithinRange(limits[0], limits[1]);
  let total = 0;
  for (let [i, hs1] of enumerate(hailstones)) {
    for (let hs2 of hailstones.slice(0, i)) {
      if (hs1.a * hs2.b === hs1.b * hs2.a) continue;
      let x = (hs1.c * hs2.b - hs2.c * hs1.b) / (hs1.a * hs2.b - hs2.a * hs1.b);
      let y = (hs2.c * hs1.a - hs1.c * hs2.a) / (hs1.a * hs2.b - hs2.a * hs1.b);
      if (isWithinRange(x) && isWithinRange(y)) {
        if ([hs1, hs2].every(hs => (x - hs.sx) * hs.vx >= 0 && (y - hs.sy) * hs.vy >= 0)) {
          total += 1;
        }
      }
    }
  }
  return total;
}

// Credit: https://github.com/ayoubzulfiqar/advent-of-code/blob/main/TypeScript/Day24/part_2.ts
function part2(input: string) {
  const hailstones: Hailstone[] = [];

  const velocitiesX: Record<number, number[]> = {};
  const velocitiesY: Record<number, number[]> = {};
  const velocitiesZ: Record<number, number[]> = {};

  let lines = input.trim().split('\n');
  for (let line of lines) {
    let hs = new Hailstone(...line.replace('@', ',').split(',').map(Number));
    velocitiesX[hs.vx] ??= [];
    velocitiesX[hs.vx].push(hs.sx);
    velocitiesY[hs.vy] ??= [];
    velocitiesY[hs.vy].push(hs.sy);
    velocitiesZ[hs.vz] ??= [];
    velocitiesZ[hs.vz].push(hs.sz);
    hailstones.push(hs);
  }

  let possibleV: number[] = [];
  for (let x = -1000; x <= 1000; x++) {
    possibleV.push(x);
  }

  let rvx = getRockVelocity(velocitiesX);
  let rvy = getRockVelocity(velocitiesY);
  let rvz = getRockVelocity(velocitiesZ);
  let results: Record<number, number> = {};
  for (let [i, stoneA] of enumerate(hailstones)) {
    for (let stoneB of hailstones.slice(i + 1)) {
      const ma = (stoneA.vy - rvy) / (stoneA.vx - rvx);
      const mb = (stoneB.vy - rvy) / (stoneB.vx - rvx);

      const ca = stoneA.sy - ma * stoneA.sx;
      const cb = stoneB.sy - mb * stoneB.sx;

      const rpx = Number((cb - ca) / (ma - mb));
      const rpy = Number(ma * rpx + ca);

      const time = Math.round((rpx - stoneA.sx) / (stoneA.vx - rvx));
      const rpz = stoneA.sz + (stoneA.vz - rvz) * time;

      const result = rpx + rpy + rpz;
      results[result] ??= 0;
      results[result]++;
    }
  }
  console.log(results);
  return Object.keys(results).sort((a, b) => results[Number(b)] - results[Number(a)])[0];
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
`;
// input = testInput;

console.log('part1:', part1(input));
// console.log('part2:', part2(input));

let arr = input
  .trim()
  .split('\n')
  .map(line => {
    let [pLit, vLit] = line.split('@');
    return {
      p: pLit.split(',').map(Number),
      v: vLit.split(',').map(Number),
    };
  });

const intersect = (a, b, shift = [0, 0], coords = [0, 1]) => {
  let x1 = a.p[coords[0]],
    y1 = a.p[coords[1]],
    x2 = a.p[coords[0]] + a.v[coords[0]] - shift[0],
    y2 = a.p[coords[1]] + a.v[coords[1]] - shift[1];
  let x3 = b.p[coords[0]],
    y3 = b.p[coords[1]],
    x4 = b.p[coords[0]] + b.v[coords[0]] - shift[0],
    y4 = b.p[coords[1]] + b.v[coords[1]] - shift[1];

  let ua,
    ub,
    denom = (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1);
  if (denom == 0) return false;
  ua = ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / denom;
  ub = ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)) / denom;
  return [x1 + ua * (x2 - x1), y1 + ua * (y2 - y1)];
};

const getTime = (a, v, shift = 0, coord = 0) => {
  return (v - a.p[coord]) / (a.v[coord] - shift);
};

// Credit: https://github.com/surgi1/adventofcode/blob/main/2023/day24/script.js

// part 2 is bruteforce
// going thru all the velocities [-500, 500]^3
// it identifies candidates and a number of matches the algo was able to verify (other rocks likely match as well, but that is harder to verify due to (I guess) belonging to the same plane)
// this is to be improved / verified

let hits = 0;
for (let vz = -500; vz <= 500; vz++) {
  // if ((vz + 1000) % 10 == 0) console.log('testing velocity z', vz);
  for (let vx = -500; vx <= 500; vx++)
    for (let vy = -500; vy <= 500; vy++) {
      // [vx, vy, vz] is the velocity of our rock
      // we substract rock velocity from each sandstone's velocity and look for common intersection
      let skip = false,
        t,
        masterInt,
        cnt = 0;

      for (let i = 0; i < arr.length; i++) {
        for (let j = i + 1; j < arr.length; j++) {
          let intXY = intersect(arr[i], arr[j], [vx, vy], [0, 1]);
          let intYZ = intersect(arr[i], arr[j], [vy, vz], [1, 2]);
          let intXZ = intersect(arr[i], arr[j], [vx, vz], [0, 2]);

          let parallelCount = [intXY, intYZ, intXZ].filter(o => o === false).length;
          if (parallelCount > 2) {
            skip = true;
            break;
          }

          let matches = 0,
            t,
            tj;

          if (intXY !== false) {
            let t1 = getTime(arr[i], intXY[0], vx, 0);
            if (isNaN(t1)) t1 = getTime(arr[i], intXY[1], vy, 1);

            let t1j = getTime(arr[j], intXY[0], vx, 0);
            if (isNaN(t1j)) t1j = getTime(arr[j], intXY[1], vy, 1);

            if (t === undefined) t = t1;
            if (tj === undefined) tj = t1j;

            if (t1 > 0 && t1j > 0 && t == t1 && tj == t1j) matches++;
          }

          if (intYZ !== false) {
            let t2 = getTime(arr[i], intYZ[0], vy, 1);
            if (isNaN(t2)) t2 = getTime(arr[i], intYZ[1], vz, 2);

            let t2j = getTime(arr[j], intYZ[0], vy, 1);
            if (isNaN(t2j)) t2j = getTime(arr[j], intYZ[1], vz, 2);

            if (t === undefined) t = t2;
            if (tj === undefined) tj = t2j;

            if (t2 > 0 && t2j > 0 && t == t2 && tj == t2j) matches++;
          }

          if (intXZ !== false) {
            let t3 = getTime(arr[i], intXZ[0], vx, 0);
            if (isNaN(t3)) t3 = getTime(arr[i], intXZ[1], vz, 2);

            let t3j = getTime(arr[j], intXZ[0], vx, 0);
            if (isNaN(t3j)) t3j = getTime(arr[j], intXZ[1], vz, 2);

            if (t === undefined) t = t3;
            if (tj === undefined) tj = t3j;

            if (t3 > 0 && t3j > 0 && t == t3 && tj == t3j) matches++;
          }

          if (matches >= 2) cnt++;
          else {
            skip = true;
            break;
          }

          if (cnt >= 20 && parallelCount == 0) {
            //console.log('a lot of intersections exist for', vx, vy, int);
            masterInt = [intXY[0], intXY[1], intYZ[1]];
          }
        }
        if (skip) break;
      }

      if (masterInt !== undefined) {
        // console.log(
        //   'possible verified point',
        //   masterInt,
        //   'velocities',
        //   vx,
        //   vy,
        //   vz,
        //   'nr of hits',
        //   cnt,
        //   'p2',
        //   masterInt.reduce((a, v) => a + v, 0)
        // );
        console.log(
          'part2:',
          masterInt.reduce((a, v) => a + v)
        );
        hits++;
      }
    }
}
