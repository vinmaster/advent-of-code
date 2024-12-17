type Input = { registers: number[]; program: number[] };

class Computer {
  registers: number[] = [];
  pointer: number = 0;
  memory: number[] = [];
  output: number[] = [];
  didJump: boolean = false;
  instructions = {
    '0': () => (this.registers[0] = Math.floor(this.registers[0] / (1 << this.getComboOperand()))),
    '1': () => (this.registers[1] = this.registers[1] ^ this.getOperand()),
    '2': () => (this.registers[1] = this.getComboOperand() & 7),
    '3': () => {
      if (this.registers[0] === 0) return;
      this.pointer = this.getOperand();
      this.didJump = true;
    },
    '4': () => (this.registers[1] = this.registers[1] ^ this.registers[2]),
    '5': () => this.output.push(this.getComboOperand() & 7),
    '6': () => (this.registers[1] = Math.floor(this.registers[0] / (1 << this.getComboOperand()))),
    '7': () => (this.registers[2] = Math.floor(this.registers[0] / (1 << this.getComboOperand()))),
  };
  loadRegisters(registers: number[]) {
    this.registers = [...registers];
  }
  getOperand() {
    return this.memory[this.pointer + 1];
  }
  getComboOperand() {
    let combo = this.memory[this.pointer + 1];
    if (combo >= 0 && combo <= 3) return combo;
    else if (combo === 4) return this.registers[0];
    else if (combo === 5) return this.registers[1];
    else if (combo === 6) return this.registers[2];
    else throw new Error('Reserved');
  }
  run(program: number[]) {
    this.memory = [...program];

    while (this.pointer < this.memory.length) {
      let opcode = this.memory[this.pointer];
      this.instructions[opcode]();
      if (opcode === 3 && this.didJump) {
        this.didJump = false;
        continue;
      }
      this.pointer += 2;
    }
    return this.output;
  }
}

export function part1(input: Input) {
  let comp = new Computer();
  comp.loadRegisters(input.registers);
  let output = comp.run(input.program);

  return output.join(',');
}

export function part2(input: Input) {
  // for (let i = 1; i < 100000000; i++) {
  //   let comp = new Computer();
  //   input.registers[0] = i;
  //   comp.loadRegisters(input.registers);
  //   let output = comp.run(input.program);
  //   if (output.join(',') === input.program.toString()) return i;
  // }

  let findInitialA = (nextVal = 0, i = input.program.length - 1): number => {
    if (i < 0) return nextVal;
    for (let aVal = nextVal * 8; aVal < nextVal * 8 + 8; aVal++) {
      let comp = new Computer();
      input.registers[0] = aVal;
      comp.loadRegisters(input.registers);
      let output = comp.run(input.program);
      if (output[0] === input.program[i]) {
        const finalVal = findInitialA(aVal, i - 1);
        if (finalVal >= 0) return finalVal;
      }
    }
    return -1;
  };

  return findInitialA();
}

function parseInput(inputString: string): Input {
  let [regStr, programStr] = inputString.trim().split('\n\n');
  let registers = regStr.split('\n').map(line => Number(line.match(/.*\: (\d+)/)!.slice(1)[0]));
  let program = programStr.split(' ')[1].split(',').map(Number);

  return { registers, program };
}

async function main(useRealInput = true) {
  let inputString = '';
  try {
    inputString =
      // @ts-expect-error: next-line
      typeof Bun !== 'undefined'
        ? // @ts-expect-error: next-line
          await Bun.file(`${import.meta.dir}/input.txt`).text()
        : // @ts-expect-error: next-line
        typeof Deno !== 'undefined'
        ? // @ts-expect-error: next-line
          await Deno.readTextFile(`${import.meta.dirname}/input.txt`)
        : '';
  } catch (error) {
    useRealInput = false;
  }

  let testInput = `
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
`;

  testInput = `
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
