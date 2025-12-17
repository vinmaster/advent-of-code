type Input = number[][];

function getCombinations<T>(elements: T[], size: number): T[][] {
  if (size === 0) return [[]];
  if (elements.length === 0) return [];
  if (size > elements.length) return [];
  let [first, ...rest] = elements;
  let combsWithFirst = getCombinations(rest, size - 1).map(comb => [first, ...comb]);
  let combsWithoutFirst = getCombinations(rest, size);
  return [...combsWithFirst, ...combsWithoutFirst];
}

function manhattanDistance(p1: number[], p2: number[]) {
  return Math.abs(p1[0] - p2[0]) + Math.abs(p1[1] - p2[1]);
}

function inRange(r1: number[], r2: number[]) {
  return (
    !(r1[0] <= r2[0] && r1[0] <= r2[1] && r1[1] <= r2[0] && r1[1] <= r2[1]) &&
    !(r1[0] >= r2[0] && r1[0] >= r2[1] && r1[1] >= r2[0] && r1[1] >= r2[1])
  );
}

function rectangleCrossesPolygon(p1: number[], p2: number[], sides: number[][][]) {
  return sides.some(
    ([[sx1, sy1], [sx2, sy2]]) =>
      inRange([sy1, sy2], [p1[1], p2[1]]) && inRange([sx1, sx2], [p1[0], p2[0]])
  );
}

export function part1(input: Input) {
  let pairs = getCombinations(input, 2);
  pairs = pairs.sort((a, b) => manhattanDistance(b[0], b[1]) - manhattanDistance(a[0], a[1]));
  let maxArea = 0;
  for (const [p1, p2] of pairs) {
    const width = Math.abs(p1[0] - p2[0]) + 1;
    const height = Math.abs(p1[1] - p2[1]) + 1;
    const area = width * height;

    if (area > maxArea) {
      maxArea = area;
    }
  }
  console.log('part1:', maxArea);
}

export function part2(input: Input) {
  let largest = 0;
  let sides = input.map((p, i) => [p, input[(i + 1) % input.length]]);
  for (let i = 0; i < input.length; i++) {
    let p1 = input[i];
    for (let j = i + 1; j < input.length; j++) {
      let p2 = input[j];
      let area = (Math.abs(p1[0] - p2[0]) + 1) * (Math.abs(p1[1] - p2[1]) + 1);
      if (area <= largest) continue;
      if (!rectangleCrossesPolygon(p1, p2, sides)) {
        largest = area;
      }
    }
  }
  console.log('part2:', largest);
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
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3`;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
  part2(parseInput(inputString));
}

// await main(false);
await main();
