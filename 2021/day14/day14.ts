function partition(array: any[], chunkSize, step = chunkSize) {
  let result: any[] = [];
  for (let i = 0; i < array.length; i += step) {
    let chunk = array.slice(i, i + chunkSize);
    if (chunk.length === chunkSize) result.push(chunk);
  }
  return result;
}

function frequencies(arr: string[]) {
  return arr.reduce(function (value, value2) {
    return value[value2] ? ++value[value2] : (value[value2] = 1), value;
  }, {});
}

function run(current: string[], rules: Record<string, string>, steps: number) {
  for (let i = 0; i < steps; i++) {
    let parts = partition(current, 2, 1);
    let last = current.at(-1);
    current = parts.flatMap(p => [p[0], rules[p.join('')]]);
    current.push(last);
  }
  let freq: Record<string, number> = frequencies(current);
  let sorted = Object.entries(freq).sort((a, b) => b[1] - a[1]);
  return sorted;
}

function part1(input: string) {
  let [template, rulesBlock] = input.trim().split('\n\n');
  let rules: Record<string, string> = {};
  for (let r of rulesBlock.split('\n')) {
    let [left, right] = r.split(' -> ');
    rules[left] = right;
  }
  let current = template.split('');
  let sorted = run(current, rules, 10);
  return sorted.at(0)[1] - sorted.at(-1)[1];
}

function part2(input: string) {
  let [template, rulesBlock] = input.trim().split('\n\n');
  let rules: Record<string, string> = {};
  for (let r of rulesBlock.trim().split('\n')) {
    let [left, right] = r.split(' -> ');
    rules[left] = right;
  }
  let current = template.split('');
  let pairs = partition(current, 2, 1);
  let letters: Record<string, number> = current.reduce((acc, cur) => {
    acc[cur] ??= 0;
    acc[cur]++;
    return acc;
  }, {});
  let counter: Record<string, number> = pairs.reduce((acc, cur) => {
    acc[cur.join('')] ??= 0;
    acc[cur.join('')] += 1;
    return acc;
  }, {});
  for (let i = 0; i < 40; i++) {
    for (let [pair, count] of Object.entries(structuredClone(counter))) {
      let insert = rules[pair];
      letters[insert] ??= 0;
      letters[insert] += count;
      counter[pair] -= count;
      counter[pair[0] + insert] ??= 0;
      counter[pair[0] + insert] += count;
      counter[insert + pair[1]] ??= 0;
      counter[insert + pair[1]] += count;
    }
  }
  let sorted = Object.entries(letters).sort((a, b) => b[1] - a[1]);
  return sorted.at(0)[1] - sorted.at(-1)[1];
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
