type Input = { keys: number[][]; locks: number[][] };

export function part1(input: Input) {
  let result = 0;
  for (let key of input.keys) {
    for (let lock of input.locks) {
      if (key.map((val, i) => val + lock[i]).every(val => val <= 5)) result += 1;
    }
  }
  return result;
}

export function part2(input: Input) {}

function parseInput(inputString: string): Input {
  let entries = inputString.trim().split('\n\n');
  let keys = [] as number[][];
  let locks = [] as number[][];
  for (let entry of entries) {
    let lines = entry.split('\n');
    if (lines[0].replaceAll('#', '').length === 0) {
      // lock
      let lock = [] as number[];
      for (let i = 0; i < lines[0].length; i++) {
        let count = 0;
        for (let height = 1; height < lines.length; height++) {
          if (lines[height][i] === '#') count += 1;
        }
        lock.push(count);
      }
      locks.push(lock);
    } else {
      // key
      let key = [] as number[];
      for (let i = 0; i < lines[0].length; i++) {
        let count = 0;
        for (let height = 0; height < lines.length - 1; height++) {
          if (lines[height][i] === '#') count += 1;
        }
        key.push(count);
      }
      keys.push(key);
    }
  }

  return { keys, locks };
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
#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
