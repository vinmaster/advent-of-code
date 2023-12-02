function part1(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let sum = 0;
  for (let line of lines) {
    let strings = line.split('');
    let first = strings.find(s => !isNaN(s));
    let last = strings.reverse().find(s => !isNaN(s));
    sum += Number(first + last);
  }
  return sum;
}

function part2(input) {
  let lines = input.trim().split('\n') as string[];
  let numbers = [null, 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'];
  let sum = 0;

  for (let line of lines) {
    let indexes = [] as [number, string][];
    for (let n of numbers.slice(1) as string[]) {
      let i = line.indexOf(n);
      while (i !== -1) {
        indexes.push([i, n]);
        i = line.indexOf(n, i + 1);
      }
    }
    let strings = line.split('');
    for (let i = 0; i < strings.length; i++) {
      if (isNaN(strings[i] as any)) continue;
      indexes.push([i, numbers[Number(strings[i])]!]);
    }
    indexes.sort((a, b) => a[0] - b[0]);
    let first = numbers.indexOf(indexes.at(0)[1]);
    let last = numbers.indexOf(indexes.at(-1)[1]);
    sum += Number(`${first}${last}`);
  }
  return sum;
}

let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

// input = `
// 1abc2
// pqr3stu8vwx
// a1b2c3d4e5f
// treb7uchet
// `;

// input = `
// two1nine
// eightwothree
// abcone2threexyz
// xtwone3four
// 4nineeightseven2
// zoneight234
// 7pqrstsixteen`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
