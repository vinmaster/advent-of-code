type Input = number[];

function blink(num: number): number[] {
  if (num === 0) return [1];
  else if (num.toString().length % 2 === 0) {
    let middle = num.toString().length / 2;
    let left = num.toString().substring(0, middle);
    let right = num.toString().substring(middle);
    return [left, right].map(Number);
  } else {
    return [num * 2024];
  }
}

function blinkFaster(dict: Record<string, number>): Record<string, number> {
  let newDict = {};
  for (let [numStr, count] of Object.entries(dict)) {
    if (numStr === '0') {
      newDict['1'] ??= 0;
      newDict['1'] += count;
    } else if (numStr.toString().length % 2 === 0) {
      let middle = numStr.length / 2;
      let left = Number(numStr.substring(0, middle));
      let right = Number(numStr.substring(middle));
      newDict[left] ??= 0;
      newDict[right] ??= 0;
      newDict[left] += count;
      newDict[right] += count;
    } else {
      newDict[Number(numStr) * 2024] ??= 0;
      newDict[Number(numStr) * 2024] = count;
    }
  }
  return newDict;
}

export function part1(input: Input) {
  let list = [...input];
  for (let i = 0; i < 25; i++) {
    list = list.flatMap(blink);
  }
  return list.length;
}

export function part2(input: Input) {
  let dict = input.reduce((acc, n) => {
    acc[n] ??= 0;
    acc[n]++;
    return acc;
  }, {} as Record<string, number>);
  let iterations = 75;
  for (let i = 0; i < iterations; i++) {
    dict = blinkFaster(dict);
  }
  return Object.values(dict).reduce((a, b) => a + b);
}

function parseInput(inputString: string): Input {
  inputString = inputString.trim();
  return inputString.trim().split(' ').map(Number);
}

async function main(useRealInput = true) {
  let inputString = '';
  try {
    inputString =
      // @ts-expect-error: next-line
      typeof Bun !== 'undefined'
        ? // @ts-expect-error: next-line
          await Bun.file(`${import.meta.dir}/input.txt`).text()
        : // @ts-expect-error: next-line
        typeof Deno !== 'undefined'
        ? // @ts-expect-error: next-line
          await Deno.readTextFile(`${import.meta.dirname}/input.txt`)
        : '';
  } catch (error) {
    useRealInput = false;
  }

  let testInput = `
125 17`;

  if (!useRealInput) inputString = testInput;

  let input = parseInput(inputString);
  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// await main(false);
await main();
