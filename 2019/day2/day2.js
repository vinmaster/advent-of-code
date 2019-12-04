const fs = require('fs');
const path = require('path');

function runOpcode(program, opcode, input1, input2, output) {
  switch (opcode) {
    case 1:
      program[output] = program[input1] + program[input2];
      break;
    case 2:
      program[output] = program[input1] * program[input2];
      break;
    case 99:
      break;
    default:
      throw new Error(`Opcode invalid: ${opcode}`);
  }
}

function runProgram(program) {
  for (let i = 0; ; i += 4) {
    const opcode = program[i];
    if (opcode === 99) { break; }
    runOpcode(program,
      opcode,
      program[i + 1],
      program[i + 2],
      program[i + 3]
    );
  }
}

function part1(input) {
  const program = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  program[1] = 12;
  program[2] = 2;
  runProgram(program);
  return program[0];
}

function part2(input) {
  let originalProgram = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  const target = 19690720;
  
  for (let noun = 0; noun <= 99; noun++) {
    for (let verb = 0; verb <= 99; verb++) {
      program = [...originalProgram];
      program[1] = noun;
      program[2] = verb;
      runProgram(program);
      if (program[0] === target) {
        return 100 * noun + verb;
      }
    }
  }
  return false;
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day2 part1:', part1(input));
console.log('day2 part2:', part2(input));
