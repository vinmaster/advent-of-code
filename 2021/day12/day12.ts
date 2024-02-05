function isLower(str: string): boolean {
  return str === str.toLowerCase();
}

function countFrequencies(arr: any[]): Record<any, number> {
  return arr.reduce((acc, x) => {
    acc[x] ??= 0;
    acc[x] += 1;
    return acc;
  }, {});
}

function collectPaths(lines: string[]): Record<string, string[]> {
  let paths: Record<string, string[]> = {};
  for (let line of lines) {
    let [node1, node2] = line.split('-');
    paths[node1] ??= [];
    paths[node2] ??= [];
    if (!paths[node1].includes(node2)) paths[node1].push(node2);
    if (!paths[node2].includes(node1)) paths[node2].push(node1);
  }
  return paths;
}

export function part1(input: string) {
  let lines = input.trim().split('\n');
  let paths = collectPaths(lines);

  let queue: [string, string[]][] = [['start', []]];
  let result: string[][] = [];

  while (queue.length !== 0) {
    let [current, visited] = queue.shift()!;
    if (current === 'end') {
      result.push([...visited, current]);
      continue;
    }
    if (!paths[current]) continue;
    let nexts = paths[current].filter(next => !(isLower(next) && visited.includes(next)));
    for (let next of nexts) {
      queue.push([next, [...visited, current]]);
    }
  }
  return result.length;
}

export function part2(input: string) {
  let lines = input.trim().split('\n');
  let paths = collectPaths(lines);

  let queue: [string, boolean, string[]][] = [['start', false, []]];
  let result: string[][] = [];

  while (queue.length !== 0) {
    let [current, didVisitSmallCaveTwice, visited] = queue.shift()!;
    if (current === 'end') {
      result.push([...visited, current]);
      continue;
    }
    if (!paths[current]) continue;
    for (let next of paths[current]) {
      if (['start'].includes(next)) continue;
      if (isLower(next)) {
        let count = countFrequencies(visited)[next] ?? 0;
        if (count < 1) {
          queue.push([next, didVisitSmallCaveTwice, [...visited, current]]);
        } else if (!didVisitSmallCaveTwice) {
          queue.push([next, true, [...visited, current]]);
        }
      } else {
        queue.push([next, didVisitSmallCaveTwice, [...visited, current]]);
      }
    }
  }
  return result.length;
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
start-A
start-b
A-c
A-b
b-d
A-end
b-end
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
