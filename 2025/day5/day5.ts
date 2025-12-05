type Input = { ranges: number[][]; ids: number[] };

function inRange(ranges: number[][], num: number) {
  return ranges.some(range => range[0] <= num && num <= range[1]);
}

export function part1(input: Input) {
  let { ranges, ids } = input;
  ranges = ranges.sort((a, b) => a[0] - b[0]);
  let freshRanges = [] as number[][];
  for (let range of ranges) {
    if (freshRanges.length === 0) {
      freshRanges.push(range);
      continue;
    }
    for (let i = 0; i < freshRanges.length; i++) {
      let range2 = freshRanges[i];
      let [_, e1] = range2;
      let [s2, e2] = range;
      if (e1 >= s2 && e2 > e1) {
        // s2 is redundant. Set e1 to max of e1 and e2
        range2[1] = e2;
      } else if (s2 > e1 && i === freshRanges.length - 1) {
        // Add to end when reaching the end
        freshRanges.push(range);
      }
    }
  }
  let freshIds = ids.filter(id => inRange(freshRanges, id));
  console.log('part1:', freshIds.length);
}

export function part2(input: Input) {
  let { ranges, ids } = input;
  ranges = ranges.sort((a, b) => a[0] - b[0]);
  let freshRanges = [] as number[][];
  for (let range of ranges) {
    if (freshRanges.length === 0) {
      freshRanges.push(range);
      continue;
    }
    for (let i = 0; i < freshRanges.length; i++) {
      let range2 = freshRanges[i];
      let [_, e1] = range2;
      let [s2, e2] = range;
      if (e1 >= s2 && e2 > e1) {
        // s2 is redundant. Set e1 to max of e1 and e2
        range2[1] = e2;
      } else if (s2 > e1 && i === freshRanges.length - 1) {
        // Add to end when reaching the end
        freshRanges.push(range);
      }
    }
  }
  let total = freshRanges.map(([s, e]) => e - s + 1).reduce((x, sum) => x + sum);
  console.log('part2:', total);
}

function parseInput(inputString: string): Input {
  let [rangesLines, idsLines] = inputString.trim().split('\n\n');
  let ranges = rangesLines.split('\n').map(line => line.split('-').map(Number));
  let ids = idsLines.split('\n').map(Number);
  return { ranges, ids };
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
3-5
10-14
16-20
12-18

1
5
8
11
17
32`;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
  part2(parseInput(inputString));
}

// await main(false);
await main();
