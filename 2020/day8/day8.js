const fs = require('fs');
const path = require('path');

class Computer {
  constructor(code) {
    this.memory = code;
    this.accumulator = 0;
    this.pointer = 0;
  }

  execute() {
    if (this.pointer === this.memory.length - 1) return this.accumulator;

    let instruction = this.memory[this.pointer];
    let [_, operation, argument] = instruction.match(/^(\w+) ([+|-]?\d+)/);

    switch (operation) {
      case 'acc':
        this.accumulator += parseInt(argument, 10);
        this.pointer += 1;
        break;
      case 'jmp':
        this.pointer += parseInt(argument, 10);
        break;
      case 'nop':
        this.pointer += 1;
        break;
      default:
        throw new Error('Invalid operation: ' + operation);
        break;
    }

    return null;
  }
}

function part1(input) {
  let lines = input
    .trim()
    .split('\n');

  let comp = new Computer(lines);
  let visited = {};
  let prev;
  while (true) {
    prev = comp.accumulator;
    comp.execute();
    if (visited[comp.pointer.toString()]) {
      return prev;
    }
    visited[comp.pointer.toString()] = true;
  }
}

function part2(input) {
  let lines = input
    .trim()
    .split('\n');

  for (let i = 0; i < lines.length; i++) {
    let instruction = lines[i];
    let [_, operation, argument] = instruction.match(/^(\w+) ([+|-]?\d+)/);
    let newLines = [...lines];
    if (operation === 'jmp') {
      newLines[i] = `nop ${argument}`;
    } else if (operation === 'nop') {
      newLines[i] = `jmp ${argument}`;
    } else {
      continue;
    }
    let comp = new Computer(newLines);
    let visited = {};
    while (true) {
      let terminateValue = comp.execute();
      if (terminateValue !== null) return terminateValue
      if (visited[comp.pointer.toString()]) {
        break;
      }
      visited[comp.pointer.toString()] = true;
    }

  }
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

console.log('day8 part1:', part1(input));
console.log('day8 part2:', part2(input));
