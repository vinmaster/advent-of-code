type Input = number[][];

function getMax(list: number[], length: number): number {
  let result = '';
  let i = 0;
  let j = list.length - length;
  while (result.length < length) {
    let part = list.slice(i, j + 1);
    i += part.indexOf(Math.max(...part));
    if (i === j) {
      result += list.slice(i).join('');
      break;
    }
    result += list[i];
    i += 1;
    j += 1;
  }
  return Number(result);
}

export function part1(input: Input) {
  let sum = 0;
  for (let bank of input) {
    let max = Math.max(...bank);
    let index = bank.indexOf(max);
    let max2 = Math.max(...bank.slice(index + 1));
    // Biggest number is at the end of the list
    if (max2 === -Infinity) {
      max2 = max;
      max = Math.max(...bank.slice(0, index));
    }
    sum += Number(`${max}${max2}`);
  }
  console.log('part1:', sum);
}

export function part2(input: Input) {
  let sum = 0;
  for (let bank of input) {
    sum += getMax(bank, 12);
  }
  console.log('part2:', sum);
}

function parseInput(inputString: string): Input {
  return inputString
    .trim()
    .split('\n')
    .map(line => line.split('').map(Number));
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
987654321111111
811111111111119
234234234234278
818181911112111`;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
  part2(parseInput(inputString));
}

// await main(false);
await main();
