type Input = string[];

function mix(secret: number, num: number) {
  return num ^ secret;
}

function mod(num1: number, num2: number) {
  return ((num1 % num2) + num2) % num2;
}

function prune(secret: number) {
  return mod(secret, 16777216);
}

function generateNextSecret(secret: number) {
  secret = prune(mix(secret, secret * 64));
  secret = prune(mix(secret, Math.trunc(secret / 32)));
  secret = prune(mix(secret, secret * 2048));
  return secret;
}

function generateSecrets(secret: number, times: number) {
  let secrets = [secret] as number[];
  for (let i = 0; i < times - 1; i++) {
    secret = generateNextSecret(secret);
    secrets.push(secret);
  }
  return secrets;
}

function getPrice(num: number): number {
  return num % 10;
}

export function part1(input: Input) {
  let numOfSecrets = 2000;
  // For sample input
  if (input.length < 100) numOfSecrets = 10;
  let secrets = [] as number[];
  for (let line of input) {
    secrets.push(generateSecrets(Number(line), numOfSecrets).at(-1)!);
  }
  return secrets.reduce((a, b) => a + b);
}

export function part2(input: Input) {
  let numOfSecrets = 2000;
  // For sample input
  if (input.length < 100) numOfSecrets = 10;
  let priceDeltaMap = {} as Record<string, number>;
  for (let line of input) {
    let secrets = [] as number[];
    let secret = Number(line);
    let v = new Set();
    secrets = generateSecrets(secret, numOfSecrets);
    for (let i = 0; i < secrets.length - 4; i++) {
      let list = secrets.slice(i, i + 5).map(getPrice);
      let deltas = [] as number[];
      for (let j = 1; j < list.length; j++) {
        deltas.push(list[j] - list[j - 1]);
      }
      let key = deltas.toString();
      if (v.has(key)) continue;
      v.add(key);
      priceDeltaMap[key] ??= 0;
      priceDeltaMap[key] += list.at(-1)!;
    }
  }
  return Math.max(...Object.values(priceDeltaMap));
}

function parseInput(inputString: string): Input {
  return inputString.trim().split('\n');
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
1
10
100
2024
`;

  testInput = `
1
2
3
2024
`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
