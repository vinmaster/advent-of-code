// deno-lint-ignore-file prefer-const

function part1(input: string) {
  let lines = input.trim().split('\n\n');

  let everyone = [];
  for (let person of lines) {
    let calories = person
      .split('\n')
      .map(n => parseInt(n, 10))
      .reduce((a, b) => a + b);
    everyone.push(calories);
  }
  let max = Math.max(...everyone);

  return max;
}

function part2(input: string) {
  let lines = input.trim().split('\n\n');

  let everyone = [];
  for (let person of lines) {
    let calories = person
      .split('\n')
      .map(n => parseInt(n, 10))
      .reduce((a, b) => a + b);
    everyone.push(calories);
  }
  everyone.sort((a, b) => b - a);
  let max = everyone.slice(0, 3).reduce((a, b) => a + b);

  return max;
}

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// 1000
// 2000
// 3000

// 4000

// 5000
// 6000

// 7000
// 8000
// 9000

// 10000`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
