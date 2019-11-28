const fs = require('fs');
const path = require('path');

const part1 = (input) => {
  let output = input;
  let polymerCancel = [];

  for (let i = 'A'.charCodeAt(); i <= 'Z'.charCodeAt(); i++) {
    const char = String.fromCharCode(i);
    polymerCancel.push(char + char.toLowerCase());
    polymerCancel.push(char.toLowerCase() + char);
  }

  return react(output, polymerCancel);
}

function react(polymer, polymerCancel) {
  let done = false;
  while (done !== true) {
    const lengthBefore = polymer.length;
    for (let cancel of polymerCancel) {
      polymer = polymer.replace(new RegExp(cancel, 'g'), '');
    }
    if (polymer.length === lengthBefore) {
      done = true
    }
  }

  return polymer.length;
}

const part2 = (input) => {
  let shortestLength = input.length;
  let polymerCancel = [];

  for (let i = 'A'.charCodeAt(); i <= 'Z'.charCodeAt(); i++) {
    const char = String.fromCharCode(i);
    polymerCancel.push(char + char.toLowerCase());
    polymerCancel.push(char.toLowerCase() + char);
  }

  for (let removeCode = 'A'.charCodeAt(); removeCode <= 'Z'.charCodeAt(); removeCode++) {
    const removeChar = String.fromCharCode(removeCode);
    let output = input.replace(new RegExp(removeChar, 'g'), '');
    output = output.replace(new RegExp(removeChar.toLowerCase(), 'g'), '');
    
    const length = react(output, polymerCancel);
    if (length < shortestLength) {
      shortestLength = length;
    }
  }

  return shortestLength;
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day5 part1', part1(input));
console.log('day5 part2', part2(input));
