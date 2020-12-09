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

/*
const input = require('../input');
const log = console.log;
const compose = (...fns) => (args) => fns.reduceRight((arg, fn) => fn(arg), args);
const match = (pattern) => (string) => string.match(pattern) ?? [];
const clone = (array) => [...array].map((item) => ({ ...item }));

const Program = () => {
  const operations = {
    acc: (argument, { current, accumulator, executed, ...state }) => ({
      current: current + 1,
      accumulator: accumulator + argument,
      executed: [...executed, current],
      ...state,
    }),
    jmp: (argument, { current, executed, ...state }) => ({
      current: current + argument,
      executed: [...executed, current],
      ...state,
    }),
    nop: (_, { current, executed, ...state }) => ({
      current: current + 1,
      executed: [...executed, current],
      ...state,
    }),
  };

  const execute = ({ operation, argument }, state) => operations[operation](argument, state);
  const isLooping = ({ current, executed }) => executed.includes(current);
  const hasTerminated = ({ current, instructions }) => current > instructions.length - 1;

  const process = (state) => {
    if (isLooping(state) || hasTerminated(state)) {
      return { value: state.accumulator, hasTerminated: hasTerminated(state) };
    }

    const instruction = state.instructions[state.current];
    state = execute(instruction, state);
    return process(state);
  };

  return {
    run: (instructions) => process({ accumulator: 0, current: 0, executed: [], instructions }),
  };
};

const parseInstruction = ([_, operation, argument]) => ({ operation, argument: +argument });
const parseInstructionFromLine = compose(parseInstruction, match(/^(acc|jmp|nop) ([+-]\d+)$/));
const instructions = input(__dirname, './input.txt').split('\n').map(parseInstructionFromLine);

const program = Program();

const resultOne = program.run(instructions);
log(`Solution pt.1 ${resultOne.value}`);

const isSwappableOperation = (operation) => ['nop', 'jmp'].includes(operation);
const swapOperation = (operation) => (operation === 'nop' ? 'jmp' : 'nop');

const resultTwo = instructions.reduce((result, { operation, argument }, index, instructions) => {
  if (result) return result;
  if (!isSwappableOperation(operation)) return null;

  instructions = clone(instructions);
  instructions[index] = { operation: swapOperation(operation), argument };
  result = program.run(instructions);

  return result.hasTerminated ? result : null;
}, null);
log(`Solution pt.2 ${resultTwo.value}`);
*/
