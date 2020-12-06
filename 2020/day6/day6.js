const fs = require('fs');
const path = require('path');

function part1(input) {
  const groups = input
    .trim()
    .split('\n\n');

  const count = groups.map(g => {
    const questions = g.split(/\s/).map(s => s.split('')).flat();
    return new Set(questions).size;
  })
  return count.reduce((cur, acc) => cur + acc);
}

function part2(input) {
  const groups = input
    .trim()
    .split('\n\n');

  const letters = groups.map(g => {
    const persons = g.split(/\s/).map(s => s.split(''));
    let result = persons.reduce((cur, acc) => {
      return cur.filter(x => acc.includes(x))
    })
    return result.length;
  })
  return letters.reduce((cur, acc) => cur + acc)
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day6 part1:', part1(input));
console.log('day6 part2:', part2(input));

/*
part 1
const data = require("fs").readFileSync("input.txt", { encoding: "utf-8" }).trim();
const groups = data.split(/\n{2,}/).map((d) => d.replace(/[^a-z]/gs, ""));
const answers = groups.map((g) => new Set([...g]));
const sum = answers.reduce((acc, a) => (acc += a.size), 0);
console.log(sum);

part 2
const data = require("fs").readFileSync("input.txt", { encoding: "utf-8" }).trim();
const groups = data.split(/\n{2,}/);
const answers = groups.map((g) => g.split(/\n/));
const alphabet = "abcdefghijklmnopqrstuvwxyz";
let sum = 0;
answers.forEach((lines) => {
  [...alphabet].forEach((char) => {
    sum += lines.every((line) => line.indexOf(char) !== -1);
  });
});
console.log(sum);
*/