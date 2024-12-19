type Input = [string[], string[]];

function matchDesign(
  patterns: string[],
  design: string,
  cache = new Map<string, [boolean, string[]]>(),
  used = [] as string[]
): [boolean, string[]] {
  // Base case, found matching
  if (design.length === 0) return [true, used];
  if (cache.has(design)) return cache.get(design)!;

  let result: [boolean, string[]] | null = null;
  for (let p of patterns) {
    if (p.length > design.length) continue;
    if (design.startsWith(p)) {
      result = matchDesign(patterns, design.slice(p.length), cache, [...used, p]);
      if (result[0]) {
        cache.set(design, result);
        return result;
      }
    }
  }
  if (!result) return [false, []];
  cache.set(design, result);
  return result;
}

function matchCount(patterns: string[], design: string, cache) {
  if (design.length === 0) return 1;
  if (cache.has(design)) return cache.get(design);
  let count = 0;

  for (const p of patterns) {
    if (p.length > design.length) continue;
    if (design.startsWith(p)) {
      count += matchCount(patterns, design.slice(p.length), cache);
    }
  }
  cache.set(design, count);
  return count;
}

export function part1(input: Input) {
  let [patterns, designs] = input;
  let result = 0;
  for (let design of designs) {
    let [matched, used] = matchDesign(patterns, design);
    if (matched) result += 1;
  }
  return result;
}

export function part2(input: Input) {
  let [patterns, designs] = input;
  let result = 0;
  let cache = new Map();
  for (let design of designs) {
    result += matchCount(patterns, design, cache);
  }
  return result;
}

function parseInput(inputString: string): Input {
  let [patternsBlock, designsBlock] = inputString.trim().split('\n\n');
  return [patternsBlock.split(', '), designsBlock.split('\n')];
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
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
