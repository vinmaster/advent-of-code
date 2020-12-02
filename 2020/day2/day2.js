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
