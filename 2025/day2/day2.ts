type Input = [number, number][];

export function part1(input: Input) {
  let invalidIds = 0;
  for (let [a, b] of input) {
    for (let i = a; i <= b; i++) {
      let half = i.toString().substring(0, i.toString().length / 2);
      if (half.repeat(2) === i.toString()) invalidIds += i;
    }
  }
  console.log('part1:', invalidIds);
}

export function part2(input: Input) {
  let invalidIds = 0;
  for (let [a, b] of input) {
    for (let i = a; i <= b; i++) {
      let s = i.toString();
      for (let len = 1; len <= s.length / 2; len++) {
        if (s.length % len !== 0) continue;
        let part = s.substring(0, len);
        let times = s.length / part.length;
        if (part.repeat(times) === s) {
          invalidIds += i;
          break;
        }
      }
    }
  }
  console.log('part2:', invalidIds);
}

function parseInput(inputString: string): Input {
  let ranges = inputString.trim().split(',');
  return ranges.map(str => {
    let [a, b] = str.split('-').map(Number);
    return [a, b];
  });
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
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124`;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
  part2(parseInput(inputString));
}

// await main(false);
await main();
