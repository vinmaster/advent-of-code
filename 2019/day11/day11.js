const fs = require('fs');
const path = require('path');

class Program {
  constructor(program, inputs) {
    this.program = program;
    this.inputs = inputs || [];
    this.outputs = [];
    this.pointer = 0;
    this.increment = 0;
    this.relativeBase = 0;
  }

  getMemory(program, address) {
    if (program.length < address) {
      while (program.length < address) {
        program.push(0);
      }
    }
    return program[address];
  }

  editMemory(program, address, value, mode, relativeBase) {
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

  run(inputs) {
    this.outputs = [];
    this.inputs = this.inputs.concat(inputs);
    for (;; this.pointer += this.increment) {
      const opcodeData = this.program[this.pointer];
      if (opcodeData === 99) { throw new Error('99'); }

      const input1 = this.getMemory(this.program, this.pointer + 1);
      const input2 = this.getMemory(this.program, this.pointer + 2);
      const input3 = this.getMemory(this.program, this.pointer + 3);
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
        param1 = this.getMemory(this.program, input1);
      }
      else if (param1Mode === 1) { param1 = input1; }
      else if (param1Mode === 2) {
        param1 = this.getMemory(this.program, this.relativeBase + input1);
      }
      else { throw new Error(`Invalid parameter mode: ${param1Mode}`); }

      let param2;
      if (param2Mode === 0) {
        param2 = this.getMemory(this.program, input2);
      }
      else if (param2Mode === 1) { param2 = input2; }
      else if (param2Mode === 2) {
        param2 = this.getMemory(this.program, this.relativeBase + input2);
      }
      else { throw new Error(`Invalid parameter mode: ${param2Mode}`); }

      switch (opcode) {
        case 1:
          this.editMemory(this.program, input3, param1 + param2, param3Mode, this.relativeBase);
          this.increment = 4;
          break;
        case 2:
          this.editMemory(this.program, input3, param1 * param2, param3Mode, this.relativeBase);
          this.increment = 4;
          break;
        case 3:
          this.editMemory(this.program, input1, this.inputs.shift(), param1Mode, this.relativeBase);
          this.increment = 2;
          break;
        case 4:
          // console.log('this.program output', param1);
          this.outputs.push(param1);
          if (this.outputs.length === 2) {
            return this.outputs;
          }
          this.increment = 2;
          break;
        case 5:
          if (param1 !== 0) {
            this.pointer = param2;
            this.increment = 0;
          } else {
            this.increment = 3;
          }
          break;
        case 6:
          if (param1 === 0) {
            this.pointer = param2;
            this.increment = 0;
          } else {
            this.increment = 3;
          }
          break;
        case 7:
          if (param1 < param2) { this.editMemory(this.program, input3, 1, param3Mode, this.relativeBase); }
          else { this.editMemory(this.program, input3, 0, param3Mode, this.relativeBase); }
          this.increment = 4;
          break;
        case 8:
          if (param1 === param2) { this.editMemory(this.program, input3, 1, param3Mode, this.relativeBase); }
          else { this.editMemory(this.program, input3, 0, param3Mode, this.relativeBase); }
          this.increment = 4;
          break;
        case 9:
          this.relativeBase += param1;
          this.increment = 2;
          break;
        case 99:
          break;
        default:
          throw new Error(`Opcode invalid: ${opcode}, ${opcodeData}`);
      }
    }
    return this.outputs;
  }
}

class Robot {
  constructor({ x, y, dir, intcode, grid }) {
    this.x = x || 0;
    this.y = y || 0;
    this.dir = dir || 0;
    this.grid = grid;
    this.program = new Program(intcode);
    this.painted = new Map();
  }

  paint(paint) {
    if (paint === 0) {
      this.grid[this.y][this.x] = '.';
    } else if (paint === 1) {
      this.grid[this.y][this.x] = '#';
    } else {
      throw new Error('Invalid paint');
    }

    const key = `${this.y},${this.x}`;
    let value = this.painted.get(key);
    if (!value) {
      value = 0;
    }
    value += 1;
    this.painted.set(key, value);
  }

  turn(turn) {
    if (turn === 0) {
      this.dir += 1;
    } else if (turn === 1) {
      this.dir -= 1;
    } else {
      throw new Error('Invalid turn');
    }
    this.dir = (this.dir + 4) % 4;
  }

  move() {
    if (this.dir === 0) {
      // Up
      this.y -= 1;
    } else if (this.dir === 1) {
      // Left
      this.x -= 1;
    } else if (this.dir === 2) {
      // Down
      this.y += 1;
    } else if (this.dir === 3) {
      // Right
      this.x += 1;
    }
  }

  run() {
    try {
      // const p = new Program(program, [grid[this.y][this.x]]);1
      for (let i = 0; i < 15199; i++) {
        const input = this.grid[this.y][this.x] === '.' ? 0 : 1;
        let [paint, turn] = this.program.run([input]);
        // console.log('output', paint, turn, input);
        this.paint(paint);
        this.turn(turn);
        this.move();
      }
    } catch (error) {
      if (error.message !== '99') {
        console.log(error);
      }
      console.log('------------------done');
    }
    
    let robot = '!';
    // console.log('dir', this.dir);
    if (this.dir === 0) {
      // Up
      robot = '^';
    } else if (this.dir === 1) {
      // Left
      robot = '<';
    } else if (this.dir === 2) {
      // Down
      robot = 'v';
    } else if (this.dir === 3) {
      // Right
      robot = '>';
    }
    if (this.y < 0 ||
      this.y > this.grid.length ||
      this.x < 0 ||
      this.x > this.grid[0].length
    ) {
      throw new Error(`Out of range ${this.x} ${this.y} of ${this.grid.length} ${this.grid[0].length}`);
    }
    this.grid[this.y][this.x] = robot;
    for (const row of this.grid) {
      // console.log(row.join(''));
    }
    // console.log([...this.painted]);
    console.log(this.painted.size);
  }
}

function part1(input) {
  const intcode = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  const rows = 5000;
  const cols = 5000;
  const grid = Array.from(Array(rows), _ => Array(cols).fill('.'));
  console.log('range', grid.length, grid[0].length);
  const robot = new Robot({
    x: 1000, y: 1000, grid, intcode,
  });
  robot.run();
  // const painted = new Map();
  // const robot = {
  //   x: 25, y: 25, dir: 0, // 0 - up, 1 - left, 2 - down, 3 - right
  // };
  
  return;
}

function part2(input) {
  const program = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  return runProgram(program, 2);
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day11 part1:', part1(input));
// console.log('day11 part2:', part2(input));
