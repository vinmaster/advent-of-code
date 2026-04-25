type Input = { indicator: number; schematics: number[]; joltage: number[] }[];

function bfsShortestPath(target: number, schematics: number[]): number {
  // Use BFS to find the shortest path from 0 to indicator, using schematics for nodes
  if (target === 0) return 0;

  let shortestPath = 0;
  let queue = [0];
  let distance = new Map<number, number>();
  distance.set(0, 0);

  while (queue.length > 0) {
    let current = queue.shift()!;
    let d = distance.get(current)!;

    for (let schematic of schematics) {
      let next = current ^ schematic;

      if (!distance.has(next)) {
        distance.set(next, d + 1);
        if (target === next) {
          shortestPath = d + 1;
        }
        queue.push(next);
      }
    }
  }
  return shortestPath;
}
function solveWithReverseDigitDP(schematics: number[], joltageTarget: number[]): number {
  const m = schematics.length; // Number of buttons
  const n = joltageTarget.length; // Number of counters/lights

  // Precompute parity states
  // Maps a parity state (bitmask) to all button combinations that produce it
  const parityStates = new Map<number, number[]>();

  for (let mask = 0; mask < 1 << m; mask++) {
    let parityState = 0;
    for (let i = 0; i < m; i++) {
      if (mask & (1 << i)) {
        parityState ^= schematics[i]; // schematics[i] is already a bitmask from your parseInput
      }
    }
    if (!parityStates.has(parityState)) {
      parityStates.set(parityState, []);
    }
    parityStates.get(parityState)!.push(mask);
  }

  const memo = new Map<string, number>();

  function countBits(num: number): number {
    let count = 0;
    while (num > 0) {
      count += num & 1;
      num >>= 1;
    }
    return count;
  }

  function dfs(current: number[]): number {
    // Base Case: We successfully reduced the target down to all 0s
    if (current.every(val => val === 0)) return 0;

    const key = current.join(',');
    if (memo.has(key)) return memo.get(key)!;

    // Find the current parity (odd/even state) of our counters
    let parityState = 0;
    for (let i = 0; i < n; i++) {
      if (current[i] % 2 !== 0) parityState |= 1 << i;
    }

    const combinations = parityStates.get(parityState) || [];
    let best = Infinity;

    // Try applying each combination that fixes the current parity
    for (const mask of combinations) {
      const nextJoltage = [...current];

      for (let i = 0; i < m; i++) {
        if (mask & (1 << i)) {
          // Subtract 1 from the joltages for every button pressed
          // We check the bits in your schematics mask to know which joltages to decrement
          for (let j = 0; j < n; j++) {
            if (schematics[i] & (1 << j)) {
              nextJoltage[j] -= 1;
            }
          }
        }
      }

      // Check if this path is valid (no negatives, and everything must now be even)
      let valid = true;
      for (const val of nextJoltage) {
        if (val < 0 || val % 2 !== 0) {
          valid = false;
          break;
        }
      }
      if (!valid) continue;

      // Halve the remaining targets and recurse
      const halved = nextJoltage.map(x => x / 2);
      const subResult = dfs(halved);

      if (subResult !== Infinity) {
        // Total = (presses to fix parity) + 2 * (presses for the halved problem)
        const total = countBits(mask) + 2 * subResult;
        if (total < best) best = total;
      }
    }

    memo.set(key, best);
    return best;
  }

  return dfs(joltageTarget);
}

export function part1(input: Input) {
  let total = 0;
  for (let { indicator, schematics } of input) {
    total += bfsShortestPath(indicator, schematics);
  }
  console.log('part1:', total);
}

export function part2(input: Input) {
  let total = 0;
  for (let { schematics, joltage } of input) {
    total += solveWithReverseDigitDP(schematics, joltage);
  }
  console.log('part2:', total);
}

function parseInput(inputString: string): Input {
  let lines = inputString.trim().split('\n');
  let regex = /\[(.*)\]\s+(.*)\s+\{(.*)\}/;

  return lines.map(line => {
    let matches = line.match(regex);
    if (!matches) throw new Error(`Invalid line format: ${line}`);
    let [, indicator, schematics, joltage] = matches;
    let schematicsArray = [] as number[];
    // Turn the schematics into an array of numbers, where each number is position of the bits that are 1
    for (let schematic of schematics.split(' ')) {
      let schematicRegex = /\d+/g;
      let indexes = [...schematic.match(schematicRegex)!.map(Number)];
      // let bits = new Array(indicator.length).fill(0);
      // for (let index of indexes) {
      //   bits[index] = 1;
      // }
      // bits.reverse();
      // schematicsArray.push(parseInt(bits.join(''), 2));

      let mask = 0;
      for (const l of indexes) mask |= 1 << l;
      schematicsArray.push(mask);
    }
    return {
      // indicator: parseInt(
      //   indicator
      //     .split('')
      //     .reverse()
      //     .map(x => (x === '#' ? '1' : '0'))
      //     .join(''),
      //   2
      // ),
      indicator: indicator.split('').reduce((acc, x, i) => (x === '#' ? acc | (1 << i) : acc), 0),
      schematics: schematicsArray,
      joltage: joltage.split(',').map(Number),
    };
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
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out`;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
  part2(parseInput(inputString));
}

// await main(false);
await main();
