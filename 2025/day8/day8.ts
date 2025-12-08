type Input = number[][];

function distance3d(p: number[], q: number[]) {
  return Math.sqrt(Math.pow(p[0] - q[0], 2) + Math.pow(p[1] - q[1], 2) + Math.pow(p[2] - q[2], 2));
}

function getCombinations<T>(elements: T[], size: number): T[][] {
  if (size === 0) return [[]];
  if (elements.length === 0) return [];
  if (size > elements.length) return [];
  let [first, ...rest] = elements;
  let combsWithFirst = getCombinations(rest, size - 1).map(comb => [first, ...comb]);
  let combsWithoutFirst = getCombinations(rest, size);
  return [...combsWithFirst, ...combsWithoutFirst];
}

function connectGroup(groups: Set<number>[], indexPairs: number[][], i: number) {
  let [id1, id2] = indexPairs[i];
  let group1Idx = groups.findIndex(set => set.has(id1));
  let group2Idx = groups.findIndex(set => set.has(id2));
  // If they are in different groups, merge them
  if (group1Idx !== group2Idx) {
    let group1 = groups[group1Idx];
    let group2 = groups[group2Idx];
    for (let val of group2) {
      group1.add(val);
    }
    groups.splice(group2Idx, 1);
  }
  // If group1Idx === group2Idx, they are already connected. Do nothing.
}

export function part1(input: Input) {
  let indices = input.map((_, i) => i);
  let indexPairs = getCombinations(indices, 2);
  indexPairs.sort(
    (a, b) => distance3d(input[a[0]], input[a[1]]) - distance3d(input[b[0]], input[b[1]])
  );
  // Get all circuits in their own group
  let groups: Set<number>[] = indices.map(i => new Set([i]));
  // Only need 10 for example input. 1000 for real input
  let target = input.length > 100 ? 1000 : 10;
  for (let i = 0; i < target; i++) {
    connectGroup(groups, indexPairs, i);
  }
  let sizes = groups.map(g => g.size).sort((a, b) => b - a);
  // console.log('Circuit sizes:', sizes);
  console.log('part1:', sizes[0] * sizes[1] * sizes[2]);
}

export function part2(input: Input) {
  let indices = input.map((_, i) => i);
  let indexPairs = getCombinations(indices, 2);
  indexPairs.sort(
    (a, b) => distance3d(input[a[0]], input[a[1]]) - distance3d(input[b[0]], input[b[1]])
  );
  let groups: Set<number>[] = indices.map(i => new Set([i]));
  let i = 0;
  while (groups.length !== 0) {
    let [id1, id2] = indexPairs[i];
    connectGroup(groups, indexPairs, i);
    if (groups.length === 1) {
      console.log('part2:', input[id1][0] * input[id2][0]);
      break;
    }
    i += 1;
  }
}

function parseInput(inputString: string): Input {
  let lines = inputString.trim().split('\n');
  return lines.map(line => line.split(',').map(Number));
}

async function main(useRealInput = true) {
  let inputString = '';
  try {
    // @ts-expect-error: next-line
    inputString = await Bun.file(`${import.meta.dir}/input.txt`).text();
  } catch (error) {
    useRealInput = false;
  }

  let testInput = `
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689`;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
  part2(parseInput(inputString));
}

// await main(false);
await main();
