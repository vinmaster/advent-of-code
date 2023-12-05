type Delta = [number, number, number];
type Mapping = { source: string; destination: string; deltas: Delta[] };
type NumRange = [number, number];

function getNextValue(mapping: Mapping, value: number): number {
  let delta = mapping.deltas.find(([start, end, _]) => value >= start && value <= end);
  let next = value;
  if (delta) {
    next = value + delta[2];
  }
  return next;
}

function partition(array: any[], chunkSize) {
  return array.reduce((resultArray, item, index) => {
    const chunkIndex = Math.floor(index / chunkSize);
    if (!resultArray[chunkIndex]) {
      resultArray[chunkIndex] = []; // start a new chunk
    }
    resultArray[chunkIndex].push(item);
    return resultArray;
  }, []);
}

function range(start: number, end?: number) {
  if (!end) {
    start = 0;
    end = start;
  }
  return [...Array(end - start).keys()].map(i => i + start);
}

function part1(input: string) {
  const blocks = input.trim().split('\n\n');
  let seedsLine = blocks[0];

  let seeds = [...seedsLine.matchAll(/(\d+)/g)].map(m => +m[0]);
  let categoryMaps = blocks.slice(1);
  let mappings = [] as Mapping[];

  for (let map of categoryMaps) {
    let lines = map.trim().split('\n');
    let [source, destination] = /^(\w+)\-to-(\w+) map.*/.exec(lines[0])!.slice(1);
    let deltas = [] as Delta[];
    for (let line of lines.slice(1)) {
      let [dest, src, r] = line.split(' ').map(Number);
      // Push [start, end, delta]
      deltas.push([src, src + r - 1, dest - src]);
    }
    // Sort by start num
    deltas.sort((a, b) => a[0] - b[0]);
    mappings.push({
      source,
      destination,
      deltas,
    });
  }

  // Starting category
  let category = 'seed';
  while (category !== 'location') {
    let mapping = mappings.find(m => m.source === category)!;
    seeds = seeds.map(seed => getNextValue(mapping, seed));
    category = mapping.destination;
  }
  return Math.min(...seeds);
}

function part2(input: string) {
  const blocks = input.trim().split('\n\n');
  let seedsLine = blocks[0];

  let nums = [...seedsLine.matchAll(/(\d+)/g)].map(m => +m[0]);
  let seedRanges = partition(nums, 2) as NumRange[];
  // Convert start/length to start/end
  seedRanges = seedRanges.map(([start, length]) => [start, start + length - 1]);
  let categoryMaps = blocks.slice(1);
  let mappings = [] as Mapping[];

  for (let map of categoryMaps) {
    let lines = map.trim().split('\n');
    let [source, destination] = /^(\w+)\-to-(\w+) map.*/.exec(lines[0])!.slice(1);
    let deltas = [] as Delta[];
    for (let line of lines.slice(1)) {
      let [dest, src, r] = line.split(' ').map(Number);
      // Push [start, end, delta]
      deltas.push([src, src + r - 1, dest - src]);
    }
    // Sort by start num
    deltas.sort((a, b) => a[0] - b[0]);
    mappings.push({
      source,
      destination,
      deltas,
    });
  }

  // Starting category
  let category = 'seed';

  // Ending category
  while (category !== 'location') {
    seedRanges.sort((a, b) => a[0] - b[0]);
    // console.log(category, seedRanges);

    let mapping = mappings.find(m => m.source === category)!;
    let newRanges = [] as NumRange[];

    for (let seedRange of seedRanges) {
      let added = false;
      for (let i = 0; i < mapping.deltas.length; i++) {
        let mappingRange = mapping.deltas[i];
        let delta = mappingRange[2];
        // Case 1: seed range start/end is before mapping range start
        if (seedRange[1] < mappingRange[0]) {
          continue;
        }
        // Case 2: seed range start/end is between mapping range start
        else if (seedRange[0] <= mappingRange[0] && mappingRange[0] <= seedRange[1]) {
          // Add seed range if it is not in a mapping range
          if (
            seedRange[0] < mappingRange[0] &&
            (i === 0 || (i > 0 && mapping.deltas[i - 1][1] < seedRange[0]))
          ) {
            newRanges.push([seedRange[0], mappingRange[0] - 1]);
          }
          newRanges.push([mappingRange[0] + delta, seedRange[1] + delta]);
          added = true;
        }
        // Case 3: seed range start/end is inside mapping range start/end
        else if (seedRange[0] > mappingRange[0] && seedRange[1] < mappingRange[1]) {
          newRanges.push([seedRange[0] + delta, seedRange[1] + delta]);
          added = true;
        }
        // Case 4: seed range start/end is between mapping range end
        else if (seedRange[0] <= mappingRange[1] && mappingRange[1] <= seedRange[1]) {
          newRanges.push([seedRange[0] + delta, mappingRange[1] + delta]);
          // Add seed range if it is not in a mapping range
          if (
            mappingRange[1] < seedRange[1] &&
            (i === mapping.deltas.length - 1 ||
              (i < mapping.deltas.length - 1 && seedRange[1] < mapping.deltas[i + 1][0]))
          ) {
            newRanges.push([mappingRange[1] + 1, seedRange[1]]);
          }
          added = true;
        }
        // Case 5: seed range start/end is after mapping range end
        else if (seedRange[0] > mappingRange[1]) {
          continue;
        }
      }
      if (!added) {
        newRanges.push(seedRange);
      }
    }
    seedRanges = newRanges;
    category = mapping.destination;
  }
  seedRanges.sort((a, b) => a[0] - b[0]);
  return [...new Set(seedRanges.map(r => r[0]))].slice(0, 5);
}

let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
