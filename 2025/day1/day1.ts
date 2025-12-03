type Input = string[];

export function part1(input: Input) {
  let dial = 50;
  let result = 0;
  for (let line of input) {
    let [dir, numStr] = line.match(/(L|R)(\d+)/)!.slice(1);
    let num = Number(numStr);
    if (dir === 'L') {
      dial = (dial - num + 100) % 100;
    } else {
      dial = (dial + num) % 100;
    }
    if (dial === 0) result += 1;
  }
  console.log('part1:', result);
}

export function part2(input: Input) {
  let dial = 50;
  let result = 0;
  for (let line of input) {
    let [dir, numStr] = line.match(/(L|R)(\d+)/)!.slice(1);
    let num = Number(numStr);
    let step = dir === 'L' ? -1 : 1;
    for (let i = 0; i < num; i++) {
      dial += step;
      dial = (dial + 100) % 100;
      if (dial === 0) result += 1;
    }
  }
  console.log('part2:', result);
}

function parseInput(inputString: string): Input {
  let lines = inputString.trim().split('\n');
  return lines;
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
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82`;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
  part2(parseInput(inputString));
}

// await main(false);
await main();
