const fs = require('fs');
const path = require('path');

function getMemory(program, address) {
  if (program.length < address) {
    while (program.length < address) {
      program.push(0);
    }
  }
  return program[address];
}

function editMemory(program, address, value, mode, relativeBase) {
  if (program.length < address) {
    while (program.length < address) {
      program.push(0);
    }
  }
  if (mode === 0) {
    program[address] = value;
  } else if (mode === 2) {
    program[address + relativeBase] = value;
  }
}

function runProgram(program, inputValue) {
  let relativeBase = 0;
  let increment = 0;
  let outputCode = null;
  for (let i = 0; ; i += increment) {
    const opcodeData = program[i];
    if (opcodeData === 99) { break; }

    const input1 = getMemory(program, i + 1);
    const input2 = getMemory(program, i + 2);
    const input3 = getMemory(program, i + 3);
    // opcodeData comes in as ABCDE

    // Get DE
    const opcode = opcodeData % 100;
    // Get C
    let param1Mode = Math.floor(opcodeData / 100) % 10;
    // Get B
    let param2Mode = Math.floor(opcodeData / 1000) % 10;
    // Get A
    let param3Mode = Math.floor(opcodeData / 10000) % 10;

    let param1;
    if (param1Mode === 0) {
      param1 = getMemory(program, input1);
    }
    else if (param1Mode === 1) { param1 = input1; }
    else if (param1Mode === 2) {
      param1 = getMemory(program, relativeBase + input1);
    }
    else { throw new Error(`Invalid parameter mode: ${param1Mode}`); }

    let param2;
    if (param2Mode === 0) {
      param2 = getMemory(program, input2);
    }
    else if (param2Mode === 1) { param2 = input2; }
    else if (param2Mode === 2) {
      param2 = getMemory(program, relativeBase + input2);
    }
    else { throw new Error(`Invalid parameter mode: ${param2Mode}`); }

    switch (opcode) {
      case 1:
        editMemory(program, input3, param1 + param2, param3Mode, relativeBase);
        increment = 4;
        break;
      case 2:
        editMemory(program, input3, param1 * param2, param3Mode, relativeBase);
        increment = 4;
        break;
      case 3:
        editMemory(program, input1, inputValue, param1Mode, relativeBase);
        increment = 2;
        break;
      case 4:
        // console.log('Program output', param1);
        outputCode = param1;
        increment = 2;
        break;
      case 5:
        if (param1 !== 0) {
          i = param2;
          increment = 0;
        } else {
          increment = 3;
        }
        break;
      case 6:
        if (param1 === 0) {
          i = param2;
          increment = 0;
        } else {
          increment = 3;
        }
        break;
      case 7:
        if (param1 < param2) { editMemory(program, input3, 1, param3Mode, relativeBase); }
        else { editMemory(program, input3, 0, param3Mode, relativeBase); }
        increment = 4;
        break;
      case 8:
        if (param1 === param2) { editMemory(program, input3, 1, param3Mode, relativeBase); }
        else { editMemory(program, input3, 0, param3Mode, relativeBase); }
        increment = 4;
        break;
      case 9:
        relativeBase += param1;
        increment = 2;
        break;
      case 99:
        break;
      default:
        throw new Error(`Opcode invalid: ${opcode}, ${opcodeData}`);
    }
  }
  return outputCode;
}

function part1(input) {
  const program = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  return runProgram(program, 1);
}

function part2(input) {
  const program = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  return runProgram(program, 2);
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day9 part1:', part1(input));
console.log('day9 part2:', part2(input));
