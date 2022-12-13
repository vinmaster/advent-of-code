// deno-lint-ignore-file prefer-const

function compare(p1: any[], p2: any[]): boolean {
  let length = Math.max(p1.length, p2.length);
  for (let i = 0; i < length; i++) {
    if (i >= p2.length) return false;
    else if (i >= p1.length) return true;

    let [v1, v2] = [p1[i], p2[i]];
    // console.log(v1, v2);

    if (!Array.isArray(v1) && !Array.isArray(v2) && v1 !== v2) {
      return v1 < v2;
    } else if (Array.isArray(v1) && Array.isArray(v2)) {
      return compare(v1, v2);
    } else if (Array.isArray(v1)) {
      return compare(v1, [v2]);
    } else if (Array.isArray(v2)) {
      return compare([v1], v2);
    }
  }

  return true;
}

function part1(input: string) {
  let entries = input.trim().split('\n\n');
  let results = [];

  for (let i = 0; i < entries.length; i++) {
    let entry = entries[i];
    let [packet1, packet2] = entry.split('\n').map(JSON.parse);
    if (compare(packet1, packet2)) {
      results.push(i + 1);
    }
  }

  return results.reduce((a, b) => a + b);
}

function part2(input: string) {
  let entries = input.trim().replaceAll('\n\n', '\n').split('\n');
  let decoderKeys = ['[[2]]', '[[6]]'];
  entries.push(...decoderKeys);

  let result = entries.map(JSON.parse);
  result.sort((a, b) => (compare(a, b) ? -1 : 1));
  result = result.map(JSON.stringify);
  let indexes = decoderKeys.map(key => result.indexOf(key) + 1);

  return indexes.reduce((a, b) => a * b);
}

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// [1,1,3,1,1]
// [1,1,5,1,1]

// [[1],[2,3,4]]
// [[1],4]

// [9]
// [[8,7,6]]

// [[4,4],4,4]
// [[4,4],4,4,4]

// [7,7,7,7]
// [7,7,7]

// []
// [3]

// [[[]]]
// [[]]

// [1,[2,[3,[4,[5,6,7]]]],8,9]
// [1,[2,[3,[4,[5,6,0]]]],8,9]`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
