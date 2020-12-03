const fs = require('fs');
const path = require('path');

const part1 = input => {
  const passwordEntries = input
    .trim()
    .split('\n');

  let validCount = 0;
  for (const passwordEntry of passwordEntries) {
    let [policy, char, password,] = passwordEntry.split(' ');
    char = char.slice(0, -1);
    const [policyMin, policyMax,] = policy.split('-');
    if (password.match(new RegExp(char, 'g'))) {
      const matches = password.match(new RegExp(char, 'g')).length;
      if (matches >= policyMin && matches <= policyMax) validCount += 1;
    }
  }
  return validCount;
};

const part2 = input => {
  const passwordEntries = input
    .trim()
    .split('\n');

  let validCount = 0;
  for (const passwordEntry of passwordEntries) {
    let [policy, char, password,] = passwordEntry.split(' ');
    char = char.slice(0, -1);
    let [index1, index2,] = policy.split('-');
    index1 -= 1;
    index2 -= 1;
    if ((password[index1] === char && password[index2] !== char) ||
      (password[index1] !== char && password[index2] === char)) {
      validCount += 1;
    }
  }
  return validCount;
};

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day2 part1:', part1(input));
console.log('day2 part2:', part2(input));

/*
proc = input.split(`\n`).map(x => {
  let [_left, query] = x.split(': ')
  let [_nums, char] = _left.split(" ")
  let [first, last] = _nums.split("-").map(Number)
  return {query, char, first, last}
})
Part 1

proc.filter(({query, char, first, last}) => {
  let change = query.length - query.replaceAll(char,"").length
  return change >= first && change <= last
}).length
Part 2

proc.filter(({query, char, first, last}) =>
  query[first-1]==char^query[last-1]==char
).length

--------------------------------------------------------------------------------

const inputs = input.split('\n').map(v => {
    const [, min, max, letter, password] = /(\d+)-(\d+) ([a-z]): ([a-z]+)/g.exec(v);
    return { min, max, letter, password };
});

result[0] = inputs.filter(({ min, max, password, letter }) => {
    const num = password.split('').filter(v => v === letter).length;
    return (num >= min) && (num <= max);
}).length;

result[1] = inputs.filter(({ min, max, password, letter }) => {
    return (password[min - 1] === letter) ^ (password[max - 1] === letter);
}).length;

--------------------------------------------------------------------------------

export const lineMatcher = /^(\d+)-(\d+) (\w): (\w+)$/;

export interface TobogganPasswordPolicy {
  low: number;
  high: number;
  letter: string;
  password: string;
}

export const parseLine = (line: string): TobogganPasswordPolicy => {
  const [, lows, highs, letter, password] = line.match(lineMatcher) ?? [];
  const low = parseInt(lows, 10);
  const high = parseInt(highs, 10);
  return { low, high, letter, password };
};

export const runner = (input: string): number =>
  split(input)
    .map(parseLine)
    .filter(({ low, high, letter, password }) => {
      const characterCount = password.split(letter).length - 1;
      return characterCount >= low && characterCount <= high;
    }).length;

if (require.main === module) {
  (async () => console.log(`Result: ${await bench(read(year, day), runner)}`))(); // 493 ~2.6ms
}
*/