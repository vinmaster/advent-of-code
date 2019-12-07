const fs = require('fs');
const path = require('path');

class Program {
  constructor(memory) {
    this.memory = memory;
    this.instructionPointer = 0;
  }
}

function runProgram(program, inputValues) {
  let outputCode = null;
  while (true) {
    const opcodeData = program.memory[program.instructionPointer];
    if (opcodeData === 99) { throw new Error('99'); }

    const input1 = program.memory[program.instructionPointer + 1];
    const input2 = program.memory[program.instructionPointer + 2];
    const output = program.memory[program.instructionPointer + 3];
    // opcodeData comes in as ABCDE

    // Get DE
    const opcode = opcodeData % 100;
    // Get C
    let param1Mode = Math.floor(opcodeData / 100) % 10;
    // Get B
    let param2Mode = Math.floor(opcodeData / 1000) % 10;
    // A is always in position mode. No code needed

    const param1 = param1Mode === 0 ? program.memory[input1] : input1;
    const param2 = param2Mode === 0 ? program.memory[input2] : input2;

    switch (opcode) {
      case 1:
        program.memory[output] = param1 + param2;
        program.instructionPointer += 4;
        break;
      case 2:
        program.memory[output] = param1 * param2;
        program.instructionPointer += 4;
        break;
      case 3:
        program.memory[input1] = inputValues.shift();
        program.instructionPointer += 2;
        break;
      case 4:
        // console.log('Program output', param1);
        program.instructionPointer += 2;
        outputCode = param1;
        return outputCode; // Pause execution
        break;
      case 5:
        if (param1 !== 0) {
          program.instructionPointer = param2;
        } else {
          program.instructionPointer += 3;
        }
        break;
      case 6:
        if (param1 === 0) {
          program.instructionPointer = param2;
        } else {
          program.instructionPointer += 3;
        }
        break;
      case 7:
        if (param1 < param2) { program.memory[output] = 1; }
        else { program.memory[output] = 0; }
        program.instructionPointer += 4;
        break;
      case 8:
        if (param1 === param2) { program.memory[output] = 1; }
        else { program.memory[output] = 0; }
        program.instructionPointer += 4;
        break;
      case 99:
        break;
      default:
        throw new Error(`Opcode invalid: ${opcode}, ${opcodeData}`);
    }
  }
  return outputCode;
}

const permute = (inputArr) => {
  let result = [];
  const permuteHelper = (arr, m = []) => {
    if (arr.length === 0) {
      result.push(m)
    } else {
      for (let i = 0; i < arr.length; i++) {
        let curr = arr.slice();
        let next = curr.splice(i, 1);
        permuteHelper(curr.slice(), m.concat(next))
      }
    }
  }
  permuteHelper(inputArr)
  return result;
}

function part1(input) {
  const program = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  const permutation = permute([0, 1, 2, 3, 4]);
  let maxSignal = null;
  for (let i = 0; i < permutation.length; i++) {
    let lastSignal = 0;
    for (let amplifier = 0; amplifier < permutation[i].length; amplifier++) {
      const phaseSetting = permutation[i][amplifier];
      lastSignal = runProgram(new Program([...program]), [phaseSetting, lastSignal]);
      if (maxSignal === null || maxSignal < lastSignal) {
        maxSignal = lastSignal;
      }
    }
  }
  return maxSignal;
}

function part2(input) {
  const program = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  let maxSignal = null;
  const permutation = permute([5, 6, 7, 8, 9]);

  for (let i = 0; i < permutation.length; i++) {
    let lastSignal = 0;
    const programs = [];
    for (let i = 0; i < 5; i++) {
      programs.push(new Program([...program]));
    }

    try {
      while (true) {
        for (let amplifier = 0; amplifier < 5; amplifier++) {
          const phaseSetting = permutation[i].shift();
          let inputValues = [];
          if (phaseSetting) {
            inputValues = inputValues.concat([phaseSetting, lastSignal]);
          } else {
            inputValues.push(lastSignal);
          }
          lastSignal = runProgram(programs[amplifier], inputValues);
          if (maxSignal === null || maxSignal < lastSignal) {
            maxSignal = lastSignal;
          }
        }
      }
    } catch (error) {
      if (!error.message === '99') {
        console.log(error);
      }
    }
  }

  return maxSignal;
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day7 part1:', part1(input));
console.log('day7 part2:', part2(input));

/*

--------------------------------------------------------------------------------


*/
