// deno-lint-ignore-file prefer-const

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
// Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
// `;

function part1(input: string) {
  let lines = input.trim().split('\n');
  let blueprints = parseInput(lines);
  return blueprints.map((b, i) => maxGeodes(b, 24) * (i + 1)).reduce((a, b) => a + b);
}

function part2(input: string) {
  let lines = input.trim().split('\n');
  let blueprints = parseInput(lines);
  return blueprints
    .slice(0, 3)
    .map(b => maxGeodes(b, 32))
    .reduce((a, b) => a * b);
}

let resourceTypes = ['ore', 'clay', 'obsidian', 'geode'] as const;
type ResourceType = typeof resourceTypes[number];
interface State {
  resources: number[];
  bots: number[];
  time: number;
}

console.log('part1:', part1(input));
console.log('part2:', part2(input));

function parseInput(lines: string[]) {
  let blueprints = [];
  for (let line of lines) {
    let matches = [...line.matchAll(/Each ([a-z]+) robot costs ([0-9a-z ]+)\./g)];
    // let blueprint = {} as Record<string, any>;
    let blueprint = [];
    for (let m of matches) {
      let match = m.slice(1);
      let cost = match[1].split(' and ').reduce((acc, entry) => {
        let [num, resource] = entry.split(' ');
        acc[resource] = Number(num);
        return acc;
      }, {} as Record<string, number>);
      // blueprint[match[0]] = cost;
      blueprint.push(resourceTypes.map(res => cost[res] || 0));
    }
    blueprints.push(blueprint);
  }
  return blueprints;
}

function maxGeodes(blueprint: number[][], maxTime: number): number {
  let max = 0;
  let queue = [] as State[];
  queue.push({
    resources: [0, 0, 0, 0],
    bots: [1, 0, 0, 0],
    time: 0,
  });

  while (queue.length > 0) {
    let { resources, bots, time } = queue.pop()!;

    for (let i = 0; i < blueprint.length; i++) {
      let costs = blueprint[i];
      let waitTime = Math.max(
        ...costs.map((cost, i) => {
          if (cost <= resources[i]) return 0;
          else if (bots[i] === 0) return maxTime + 1;
          return Math.floor((cost - resources[i] + bots[i] - 1) / bots[i]);
        })
      );

      let newTime = time + waitTime + 1;
      if (newTime >= maxTime) continue;

      let newResources = [0, 4];
      for (let j = 0; j < bots.length; j++) {
        newResources[j] = resources[j] + bots[j] * (waitTime + 1) - costs[j];
      }
      let newBots = bots.slice();
      newBots[i] += 1;

      // build geode bots, if not greater than max, skip
      let remainingTime = maxTime - newTime;
      if (
        ((remainingTime - 1) * remainingTime) / 2 + newResources[3] + remainingTime * newBots[3] <
        max
      )
        continue;

      queue.push({ resources: newResources, bots: newBots, time: newTime });
    }

    let geodes = resources[3] + bots[3] * (maxTime - time);
    max = Math.max(max, geodes);
  }

  return max;
}
