// deno-lint-ignore-file prefer-const

export function part1(input: string) {
  input = input.trim();
  let result = 0;
  for (let line of input.split('\n')) {
    let nums = line.split(' ').map(Number);

    if (isSafe(nums)) result += 1;
  }
  return result;
}

export function part2(input: string) {
  input = input.trim();
  let result = 0;
  for (let line of input.split('\n')) {
    let nums = line.split(' ').map(Number);

    if (isSafe(nums)) result += 1;
    else {
      for (let i = 0; i < nums.length; i++) {
        let copy = [...nums];
        copy.splice(i, 1);
        if (isSafe(copy)) {
          result += 1;
          break;
        }
      }
    }
  }
  return result;
}

function isSafe(nums: number[]): boolean {
  let diff = 0;
  for (let i = 1; i < nums.length; i++) {
    if (diff !== 0 && Math.sign(diff) !== Math.sign(nums[i - 1] - nums[i])) {
      return false;
    }
    diff = nums[i - 1] - nums[i];
    if (Math.abs(diff) < 1 || Math.abs(diff) > 3) {
      return false;
    }
  }
  return true;
}

async function main(useRealInput = true) {
  let input =
    // @ts-ignore: next-line
    typeof Bun !== 'undefined'
      ? // @ts-ignore: next-line
        await Bun.file(`${import.meta.dir}/input.txt`).text()
      : typeof Deno !== 'undefined'
      ? await Deno.readTextFile(`${import.meta.dirname}/input.txt`)
      : '';

  let testInput = `
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
`;
  if (!useRealInput) input = testInput;

  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// main(false);
main();
