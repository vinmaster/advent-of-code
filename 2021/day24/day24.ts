type Input = string[];

function getConstraints(input: Input) {
  const constraints: {
    popIndex: number;
    pushIndex: number;
    diff: number;
  }[] = [];

  const stack: { index: number; C: number }[] = [];
  const CHUNK_SIZE = 18;
  const NUM_CHUNKS = 14;

  for (let i = 0; i < NUM_CHUNKS; i++) {
    const chunkLines = input.slice(i * CHUNK_SIZE, (i + 1) * CHUNK_SIZE);

    const A = parseInt(chunkLines[4].split(' ')[2]);
    const B = parseInt(chunkLines[5].split(' ')[2]);
    const C = parseInt(chunkLines[15].split(' ')[2]);

    if (A === 1) {
      stack.push({ index: i, C: C });
    } else {
      const lastPush = stack.pop();

      if (lastPush) {
        // The constraint is: w_pop = w_push + lastPush.C + B
        // We store this relationship
        constraints.push({
          popIndex: i,
          pushIndex: lastPush.index,
          diff: lastPush.C + B,
        });
      }
    }
  }
  return constraints;
}

export function part1(input: Input): string {
  // Find the LARGEST valid model number
  const constraints = getConstraints(input);
  const digits = new Array(14).fill(0);

  for (const { popIndex, pushIndex, diff } of constraints) {
    // Constraint: digits[popIndex] = digits[pushIndex] + diff
    // To maximize, we set one of the digits to 9.
    if (diff > 0) {
      // e.g., w[5] = w[4] + 3
      // To maximize, set w[5] = 9, which makes w[4] = 6
      digits[popIndex] = 9;
      digits[pushIndex] = 9 - diff;
    } else {
      // e.g., w[7] = w[6] - 4 (diff = -4)
      // To maximize, set w[6] = 9, which makes w[7] = 5
      digits[pushIndex] = 9;
      digits[popIndex] = 9 + diff;
    }
  }
  return digits.join('');
}

export function part2(input: Input): string {
  const constraints = getConstraints(input);
  const digits = new Array(14).fill(0);

  for (const { popIndex, pushIndex, diff } of constraints) {
    // Constraint: digits[popIndex] = digits[pushIndex] + diff
    // To minimize, we set one of the digits to 1.
    if (diff > 0) {
      // e.g., w[5] = w[4] + 3
      // To minimize, set w[4] = 1, which makes w[5] = 4
      digits[pushIndex] = 1;
      digits[popIndex] = 1 + diff;
    } else {
      // e.g., w[7] = w[6] - 4 (diff = -4)
      // To minimize, set w[7] = 1, which makes w[6] = 5
      digits[popIndex] = 1;
      digits[pushIndex] = 1 - diff;
    }
  }
  return digits.join('');
}

function parseInput(inputString: string): Input {
  return inputString.trim().split('\n').filter(Boolean);
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
          await Deno.readTextFile(new URL('./input.txt', import.meta.url).pathname)
        : '';
  } catch (error) {
    useRealInput = false;
  }

  if (inputString.length > 0) useRealInput = true;

  const parsedInput = parseInput(inputString);

  console.log('part1:', part1(parsedInput));
  console.log('part2:', part2(parsedInput));
}

await main();
