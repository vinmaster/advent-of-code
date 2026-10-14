type Input = Record<string, string[]>;

export function part1(input: Input) {
  let map = { out: 1 } as Record<string, number>;
  let allDevices = new Set<string>(Object.keys(input));
  while (allDevices.symmetricDifference(new Set(Object.keys(map))).size > 1) {
    for (let [device, outputs] of Object.entries(input)) {
      if (map[device] !== undefined) continue;
      if (outputs.some(output => map[output] === undefined)) continue;
      map[device] ??= 0;
      // map[device] += map[output];
      map[device] += outputs.reduce((sum, output) => sum + map[output], 0);
    }
    // console.log(map, Object.keys(map));
  }
  let totalPaths = 0;
  for (let output of input['you'] ?? []) {
    totalPaths += map[output];
  }
  console.log('part1:', totalPaths);
}

export function part2(input: Input) {
  let startNode: string = 'svr';
  let endNode: string = 'out';
  let requiredNodes: string[] = ['dac', 'fft'];
  let inDegree: Record<string, number> = {};
  let nodes = new Set<string>();

  for (let [u, neighbors] of Object.entries(input)) {
    nodes.add(u);
    for (let v of neighbors) {
      nodes.add(v);
      inDegree[v] = (inDegree[v] || 0) + 1;
    }
  }

  let queue: string[] = [];
  for (let node of nodes) {
    if (!inDegree[node]) queue.push(node);
  }

  let topoOrder: string[] = [];
  while (queue.length > 0) {
    let u = queue.shift()!;
    topoOrder.push(u);

    for (let v of input[u] || []) {
      inDegree[v]--;
      if (inDegree[v] === 0) {
        queue.push(v);
      }
    }
  }

  function countPaths(start: string, end: string): number {
    let ways: Record<string, number> = { [start]: 1 };
    let started = false;

    for (let u of topoOrder) {
      if (u === start) started = true;
      if (!started) continue;
      if (u === end) break;

      if (ways[u]) {
        for (let v of input[u] || []) {
          ways[v] = (ways[v] || 0) + ways[u];
        }
      }
    }
    return ways[end] || 0;
  }

  function getPermutations(arr: string[]): string[][] {
    if (arr.length === 0) return [[]];
    let result: string[][] = [];
    for (let i = 0; i < arr.length; i++) {
      let rest = getPermutations([...arr.slice(0, i), ...arr.slice(i + 1)]);
      for (let p of rest) {
        result.push([arr[i], ...p]);
      }
    }
    return result;
  }

  let permutations = getPermutations(requiredNodes);
  let totalPaths = 0;

  for (let perm of permutations) {
    // Construct the full sequence: [start] -> [req1] -> [req2] -> ... -> [end]
    let sequence = [startNode, ...perm, endNode];
    let currentPermutationPaths = 1;

    for (let i = 0; i < sequence.length - 1; i++) {
      let pathsBetween = countPaths(sequence[i], sequence[i + 1]);
      currentPermutationPaths *= pathsBetween;

      // Optimization: If a segment has 0 paths, this entire permutation is invalid
      if (currentPermutationPaths === 0) break;
    }

    totalPaths += currentPermutationPaths;
  }

  console.log(`part2:`, totalPaths);
}

function parseInput(inputString: string): Input {
  let lines = inputString.trim().split('\n');
  let regex = /\w+/g;
  return lines.reduce((map, line) => {
    let match = line.match(regex)!;
    if (!match) throw new Error(`Invalid line: ${line}`);
    map[match[0]] = match.slice(1);
    return map;
  }, {} as Input);
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

  testInput = `
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out`;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
  part2(parseInput(inputString));
}

// await main(false);
await main();
