function overlap(a: number[], b: number[]) {
  return (
    Math.max(a[0], b[0]) <= Math.min(a[3], b[3]) && Math.max(a[1], b[1]) <= Math.min(a[4], b[4])
  );
}

function part1(input: string) {
  let bricks = input
    .trim()
    .split('\n')
    .map(row => row.replace('~', ',').split(',').map(Number));

  // Sort by z value
  bricks.sort((a, b) => a[2] - b[2]);

  // Drop bricks
  for (let i = 0; i < bricks.length; i++) {
    let maxZ = 1;
    let brick = bricks[i];
    for (let check of bricks.slice(0, i)) {
      if (overlap(brick, check)) {
        maxZ = Math.max(maxZ, check[5] + 1);
      }
    }
    brick[5] -= brick[2] - maxZ;
    brick[2] = maxZ;
  }
  bricks.sort((a, b) => a[2] - b[2]);

  let kSupportsV: Record<string, Set<number>> = {};
  let vSupportsK: Record<string, Set<number>> = {};
  for (let i = 0; i < bricks.length; i++) {
    kSupportsV[i] ??= new Set<number>();
    vSupportsK[i] ??= new Set<number>();
  }

  for (let j = 0; j < bricks.length; j++) {
    let upper = bricks[j];
    let lowers = bricks.slice(0, j);
    for (let i = 0; i < lowers.length; i++) {
      let lower = lowers[i];
      if (overlap(lower, upper) && upper[2] === lower[5] + 1) {
        kSupportsV[i].add(j);
        vSupportsK[j].add(i);
      }
    }
  }

  let total = 0;
  for (let i = 0; i < bricks.length; i++) {
    let canDisintegrated = true;
    for (let j of kSupportsV[i]) {
      if (vSupportsK[j].size < 2) {
        canDisintegrated = false;
      }
    }
    if (canDisintegrated) total += 1;
  }

  // console.log(bricks);
  return total;
}

function isSubset<T>(a: Set<T>, b: Set<T>): boolean {
  return [...a.values()].every(element => b.has(element));
}

function part2(input: string) {
  let bricks = input
    .trim()
    .split('\n')
    .map(row => row.replace('~', ',').split(',').map(Number));

  // Sort by z value
  bricks.sort((a, b) => a[2] - b[2]);

  // Drop bricks
  for (let i = 0; i < bricks.length; i++) {
    let maxZ = 1;
    let brick = bricks[i];
    for (let check of bricks.slice(0, i)) {
      if (overlap(brick, check)) {
        maxZ = Math.max(maxZ, check[5] + 1);
      }
    }
    brick[5] -= brick[2] - maxZ;
    brick[2] = maxZ;
  }
  bricks.sort((a, b) => a[2] - b[2]);

  let kSupportsV: Record<string, Set<number>> = {};
  let vSupportsK: Record<string, Set<number>> = {};
  for (let i = 0; i < bricks.length; i++) {
    kSupportsV[i] ??= new Set<number>();
    vSupportsK[i] ??= new Set<number>();
  }

  for (let j = 0; j < bricks.length; j++) {
    let upper = bricks[j];
    let lowers = bricks.slice(0, j);
    for (let i = 0; i < lowers.length; i++) {
      let lower = lowers[i];
      if (overlap(lower, upper) && upper[2] === lower[5] + 1) {
        kSupportsV[i].add(j);
        vSupportsK[j].add(i);
      }
    }
  }

  let total = 0;
  for (let i = 0; i < bricks.length; i++) {
    let queue = [...kSupportsV[i].values()].filter(j => vSupportsK[j].size === 1);
    let falling = new Set(queue);
    falling.add(i);
    while (queue.length > 0) {
      let j = queue.shift()!;
      for (let k of kSupportsV[j]) {
        if (!falling.has(k) && isSubset(vSupportsK[k], falling)) {
          queue.push(k);
          falling.add(k);
        }
      }
    }
    total += falling.size - 1;
  }

  // console.log(bricks);
  return total;
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
