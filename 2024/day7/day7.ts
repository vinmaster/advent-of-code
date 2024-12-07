type Input = { testValue: number; nums: number[] }[];

function getPermutations(array: any[], size: number) {
  if (size <= 0) return [];
  let result: any[][] = [];
  let stack: any[][] = [];
  stack.push([]);
  while (stack.length > 0) {
    let combination = stack.pop()!;
    if (combination.length === size) {
      result.push(combination);
      continue;
    }
    for (let i = 0; i < array.length; i++) {
      stack.push([...combination, array[i]]);
    }
  }
  return result;
}

function calculate(nums: number[], operators: string[]): number {
  let total = nums[0];
  for (let i = 1; i < nums.length; i++) {
    if (operators[i - 1] === '+') {
      total += nums[i];
    } else if (operators[i - 1] === '*') {
      total *= nums[i];
    } else if (operators[i - 1] === '||') {
      total = total * Math.pow(10, nums[i].toString().length) + nums[i];
    }
  }
  return total;
}

function parseInput(inputString: string): Input {
  inputString = inputString.trim();
  return inputString.split('\n').map(line => {
    let [testValue, ...nums] = [...line.matchAll(new RegExp('(\\d+)', 'g'))].map(m => Number(m[0]));
    return { testValue, nums };
  });
}

export function part1(input: Input) {
  let operators = ['+', '*'];
  return input
    .filter(entry => {
      let permutations = getPermutations(operators, entry.nums.length - 1);
      return permutations.some(p => calculate(entry.nums, p) === entry.testValue);
    })
    .reduce((sum, entry) => sum + entry.testValue, 0);
}

export function part2(input: Input) {
  let operators = ['+', '*', '||'];
  return input
    .filter(entry => {
      let permutations = getPermutations(operators, entry.nums.length - 1);
      return permutations.some(p => calculate(entry.nums, p) === entry.testValue);
    })
    .reduce((sum, entry) => sum + entry.testValue, 0);
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
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
`;
  if (!useRealInput) inputString = testInput;

  let input = parseInput(inputString);
  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// await main(false);
await main();
