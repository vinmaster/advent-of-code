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
    this.exit = false;
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
    this.inputs = inputs;
    for (; !this.exit; this.pointer += this.increment) {
      const opcodeData = this.program[this.pointer];

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
          let input = this.inputs.shift();
          this.editMemory(this.program, input1, input, param1Mode, this.relativeBase);
          this.inputs.push(input);
          this.increment = 2;
          break;
        case 4:
          this.outputs.push(param1);
          this.increment = 2;
          this.pointer += this.increment;
          return this.outputs;
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
          this.exit = true;
          break;
        default:
          throw new Error(`Opcode invalid: ${opcode}, ${opcodeData}`);
      }
    }
  }
}

class Robot {
  constructor({ x, y, dir, intcode, startingPanel = '.' }) {
    this.x = x || 0;
    this.y = y || 0;
    this.dir = dir || 0;
    this.grid = {};
    this.grid[`${this.x},${this.y}`] = startingPanel;
    this.program = new Program(intcode);
    this.painted = new Map();
  }

  get position() {
    return `${this.x},${this.y}`;
  }

  get currentPanel() {
    return this.grid[this.position];
  }

  get direction() {
    if (this.dir === 0) {
      return 'Up';
    } else if (this.dir === 1) {
      return 'Left';
    } else if (this.dir === 2) {
      return 'Down';
    } else if (this.dir === 3) {
      return 'Right';
    }
  }

  paint(paint) {
    if (paint === 0) {
      this.grid[this.position] = '.';
    } else if (paint === 1) {
      this.grid[this.position] = '#';
    } else {
      throw new Error('Invalid paint');
    }

    let value = this.painted.get(this.position);
    if (!value) {
      value = 0;
    }
    value += 1;
    this.painted.set(this.position, value);
  }

  turn(turn) {
    let LEFT = 0;
    let RIGHT = 1;
    if (turn === LEFT) {
      this.dir += 1;
    } else if (turn === RIGHT) {
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
    for (let i = 0; !this.program.exit; i++) {
      const input = this.currentPanel === '#' ? 1 : 0;
      this.program.run([input]);
      if (this.program.outputs.length > 1) {
        let [paint, turn] = [this.program.outputs.shift(), this.program.outputs.shift()];

        this.paint(paint);
        this.turn(turn);
        this.move();
      }
    }
  }

  getGridStr() {
    let coords = Object.keys(this.grid).map(c => c.split(',').map(v => parseInt(v, 10)));
    let xs = coords.map(c => c[0]);
    let ys = coords.map(c => c[1]);

    xs.sort((a, b) => a - b);
    ys.sort((a, b) => a - b);

    let min_x = xs[0];
    let max_x = xs[xs.length - 1];
    let min_y = ys[0];
    let max_y = ys[ys.length - 1];

    let str = '\n';
    for (let y = min_y; y <= max_y; y++) {
      for (let x = min_x; x <= max_x; x++) {
        let cell = `${x},${y}`;
        str += this.grid[cell] === '#' ? '#' : ' ';
      }
      str += '\n';
    }

    return str;
  }
}

function part1(input) {
  const intcode = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  const robot = new Robot({
    x: 0, y: 0, intcode,
  });
  robot.run();

  return robot.painted.size;
}

function part2(input) {
  const intcode = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  const robot = new Robot({
    x: 0, y: 0, intcode, startingPanel: '#',
  });
  robot.run();

  return robot.getGridStr();
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day11 part1:', part1(input));
console.log('day11 part2:', part2(input));

/*
// Part 1
const { input } = require('./input');
const { Ship } = require('./intcode-computer');

let ship = new Ship(input);
let output = ship.run();

console.log(output);
// Part 2
const { input } = require('./input');
const { Ship } = require('./intcode-computer');

let ship = new Ship(input, 1);
ship.run();
ship.printShip();

// intcode-computer.js
const ADD = '01'; // Add
const MUL = '02'; // Multiply
const INP = '03'; // Input
const OUT = '04'; // Output
const JIT = '05'; // Jump-if-true
const JIF = '06'; // Jump-if-false
const LTH = '07'; // Less Than
const EQU = '08'; // Equals
const ARB = '09'; // Adjust relative base
const STP = '99'; // Stop

const POSITION_MODE = '0';
const IMMEDIATE_MODE = '1';
const RELATIVE_MODE = '2';

class Computer {
  constructor(
    memory,
    inputs,
    replenish_input,
    pause_on_output = true,
    id = 0,
    clone_memory = false
  ) {
    // For debugging
    this.id = String.fromCharCode('A'.charCodeAt(0) + id);

    this.original_memory = clone_memory && memory.slice(0);
    this.memory = memory.slice(0);
    this.pointer = 0;
    this.relative_base = 0;
    this.pause_on_output = pause_on_output;

    this.inputs = Array.isArray(inputs) ? inputs.slice(0) : [inputs];
    this.replenish_input = replenish_input;
    this.outputs = [];

    this.OPS = {
      [ADD]: {
        name: ADD,
        realName: 'ADD',
        params: 3,
        fn: (a, b, c) => {
          this.memory[c] = a + b;
        },
        write: true,
      },

      [MUL]: {
        name: MUL,
        realName: 'MUL',
        params: 3,
        fn: (a, b, c) => {
          this.memory[c] = a * b;
        },
        write: true,
      },

      [INP]: {
        name: INP,
        realName: 'INP',
        params: 1,
        fn: a => {
          this.memory[a] = this.inputs.shift();
          if (this.replenish_input !== undefined) {
            this.inputs.push(this.replenish_input);
          }
        },
        write: true,
      },

      [OUT]: {
        name: OUT,
        realName: 'OUT',
        params: 1,
        fn: a => this.output(a),
      },

      [ARB]: {
        name: ARB,
        realName: 'ARB',
        params: 1,
        fn: a => (this.relative_base += a),
      },

      [STP]: {
        name: STP,
        realName: 'STP',
        params: 0,
        fn: () => (this.halted = true),
      },

      [JIT]: {
        name: JIT,
        realName: 'JIT',
        params: 2,
        fn: (a, b) => {
          if (a) {
            this.pointer = b;
            return true;
          }
          return false;
        },
        jumps: true,
      },

      [JIF]: {
        name: JIF,
        realName: 'JIF',
        params: 2,
        fn: (a, b) => {
          if (!a) {
            this.pointer = b;
            return true;
          }
          return false;
        },
        jumps: true,
      },

      [LTH]: {
        name: LTH,
        realName: 'LTH',
        params: 3,
        fn: (a, b, c) => {
          this.memory[c] = a < b ? 1 : 0;
        },
        write: true,
      },

      [EQU]: {
        name: EQU,
        realName: 'EQU',
        params: 3,
        fn: (a, b, c) => {
          this.memory[c] = a === b ? 1 : 0;
        },
        write: true,
      },
    };

    this.halted = false;
  }

  run() {
    let op = this.parseOp();

    while (!this.halted) {
      this.runOp(op);

      if ((this.pause_on_output && op.name === OUT) || this.halted) {
        break;
      }

      op = this.parseOp();
    }

    return this.outputs;
  }

  parseOp() {
    let temp_op = String(this.memory[this.pointer]).padStart(2, '0');

    // "The opcode is a two-digit number based only on the ones and tens digit of the value, that is, the opcode is the rightmost two digits of the first value in an instruction"
    let op = this.OPS[temp_op.substr(-2, 2)];

    let full_op = temp_op.padStart(op.params + 2, '0');

    let modes = [];

    for (let i = op.params - 1; i >= 0; i--) {
      modes.push(full_op[i]);
    }

    return {
      ...op,
      modes,
    };
  }

  runOp({ modes, fn, jumps, write }) {
    this.pointer++;
    let values = [];
    for (let i = 0; i < modes.length; i++) {
      let mode = modes[i];
      let value = this.memory[this.pointer + i];

      // Values can overflow existing memory. If so, value should be 0
      if (value === undefined) {
        value = 0;
      }

      // Immediate Mode uses the value as is, no need to make adjustments
      if (mode !== IMMEDIATE_MODE) {
        const can_switch_to_position = !write || i < modes.length - 1;

        if (can_switch_to_position && mode === POSITION_MODE) {
          value = this.memory[value];
        } else if (mode === RELATIVE_MODE) {
          if (can_switch_to_position) {
            value = this.memory[value + this.relative_base];
          } else {
            value = value + this.relative_base;
          }
        }
      }

      // After remapping (if any) again adjust to 0 if we overflowed our memory read
      if (value === undefined) {
        value = 0;
      }

      values.push(value);
    }

    // If result is `true`, we moved the pointer
    let result = fn(...values);

    if (!jumps || (jumps && !result)) {
      this.pointer += modes.length;
    }
  }

  output(v) {
    this.outputs.push(v);
  }

  // For debugging
  get _() {
    return this.memory.slice(Math.max(0, this.pointer - 1), this.pointer + 8);
  }
}

class Direction {
  constructor(x, y) {
    this.x = x;
    this.y = y;
    this.direction = [0, -1];
  }

  rotateLeft() {
    const [x, y] = this.direction;
    if (x === 0 && y === -1) {
      this.direction = [-1, 0];
    } else if (x === -1 && y === 0) {
      this.direction = [0, 1];
    } else if (x === 0 && y === 1) {
      this.direction = [1, 0];
    } else if (x === 1 && y === 0) {
      this.direction = [0, -1];
    }

    this.move();
  }

  rotateRight() {
    const [x, y] = this.direction;
    if (x === 0 && y === -1) {
      this.direction = [1, 0];
    } else if (x === 1 && y === 0) {
      this.direction = [0, 1];
    } else if (x === 0 && y === 1) {
      this.direction = [-1, 0];
    } else if (x === -1 && y === 0) {
      this.direction = [0, -1];
    }

    this.move();
  }

  move() {
    const [x, y] = this.direction;
    this.x += x;
    this.y += y;
  }

  get coord() {
    return `${this.x},${this.y}`;
  }
}

class Ship {
  constructor(memory, starting_color = 0) {
    this.computer = new Computer(memory, [starting_color], starting_color);

    this.ship = { '0,0': starting_color };
    this.direction = new Direction(0, 0);
  }

  run() {
    let computer = this.computer;
    let output;

    while (!computer.halted) {
      output = computer.run();
      if (computer.halted) {
        break;
      }

      if (output.length > 1) {
        // 0 is black, 1 is white
        let color = output.shift();
        // 0 is turn left, 1 is turn right
        let direction = output.shift();

        this.ship[this.direction.coord] = color;

        if (direction === 0) {
          this.direction.rotateLeft();
        } else if (direction === 1) {
          this.direction.rotateRight();
        } else {
          console.log('ERRROR', direction);
        }

        // Set input replenish after move
        let new_color = this.ship[this.direction.coord] || 0;
        computer.inputs = [new_color];
        computer.replenish_input = new_color;
      }
    }

    return Object.keys(this.ship).length;
  }

  printShip() {
    let coords = Object.keys(this.ship).map(c => c.split(',').map(v => parseInt(v, 10)));
    let xs = coords.map(c => c[0]);
    let ys = coords.map(c => c[1]);

    xs.sort((a, b) => a - b);
    ys.sort((a, b) => a - b);

    let min_x = xs[0];
    let max_x = xs[xs.length - 1];
    let min_y = ys[0];
    let max_y = ys[ys.length - 1];

    let str = '';
    for (let y = min_y; y <= max_y; y++) {
      for (let x = min_x; x <= max_x; x++) {
        let cell = `${x},${y}`;
        str += this.ship[cell] ? '#' : ' ';
      }
      str += '\n';
    }
    console.log(str);
  }
}

module.exports = {
  Computer,
  Ship,
};
*/