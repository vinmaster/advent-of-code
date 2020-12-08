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

const sleep = (ms = 1000) => new Promise(r => setTimeout(r, ms));

class Game {
  static EMPTY = 0;
  static WALL = 1;
  static BLOCK = 2;
  static PADDLE = 3;
  static BALL = 4;
  static JOYSTICK = {
    NEUTRAL: 0,
    LEFT: -1,
    RIGHT: 1,
  }

  constructor({ intcode }) {
    this.program = new Program(intcode);
    this.tiles = {};
    this.input = Game.JOYSTICK.NEUTRAL;
    this.score = 0;
    this.ball = null;
    this.paddle = null;
  }

  getBlockTiles() {
    for (let i = 0; !this.program.exit; i++) {
      this.program.run([]);
      if (this.program.outputs.length > 2) {
        let [x, y, num] = [
          this.program.outputs.shift(),
          this.program.outputs.shift(),
          this.program.outputs.shift(),
        ];
        // Num is tile id
        let coord = `${x},${y}`;
        this.tiles[coord] = num;
      }
    }
    return Object.values(this.tiles).filter(t => t === Game.BLOCK).length
  }

  run() {
    for (let i = 0; !this.program.exit; i++) {
      this.program.run([]);
      if (this.program.outputs.length > 2) {
        let [x, y, num] = [
          this.program.outputs.shift(),
          this.program.outputs.shift(),
          this.program.outputs.shift(),
        ];

        // Num is tile id
        let coord = `${x},${y}`;
        this.tiles[coord] = num;
      }
    }
  }

  async runFreePlay() {
    for (let i = 0; !this.program.exit; i++) {
      this.program.run([this.input]);
      if (this.program.outputs.length > 2) {
        let [x, y, num] = [
          this.program.outputs.shift(),
          this.program.outputs.shift(),
          this.program.outputs.shift(),
        ];

        if (x === -1 && y === 0) {
          // Num is score
          this.score = num;
        } else {
          // Num is tile id
          let coord = `${x},${y}`;
          this.tiles[coord] = num;

          // This is to print the game being played
          // if ([Game.BALL, Game.PADDLE].includes(num)) {
          //   console.log(this.getDisplay());
          //   await sleep();
          // }

          if (num === Game.BALL) this.ball = { x, y };
          if (num === Game.PADDLE) this.paddle = { x, y };

          if (this.ball && this.paddle) {
            if (this.ball.x > this.paddle.x) this.input = Game.JOYSTICK.RIGHT;
            else if (this.ball.x < this.paddle.x) this.input = Game.JOYSTICK.LEFT;
            else this.input = Game.JOYSTICK.NEUTRAL;
          }
        }
      }
    }
  }

  getDisplay() {
    let coords = Object.keys(this.tiles)
      .map(s => s.split(',').map(pos => parseInt(pos, 10)));
    let xs = coords.map(([x, y]) => x);
    let ys = coords.map(([x, y]) => y);
    let maxX = Math.max(...xs);
    let minX = Math.min(...xs);
    let maxY = Math.max(...ys);
    let minY = Math.min(...ys);

    let scoreStr = `Score: ${this.score} \n`
    let str = scoreStr.padStart(((maxX - minX) / 2) + (scoreStr.length / 2));
    for (let row = minY; row <= maxY; row++) {
      for (let col = minX; col <= maxX; col++) {
        let key = `${col},${row}`;
        switch (this.tiles[key]) {
          case Game.EMPTY:
            str += ' ';
            break;
          case Game.WALL:
            str += '|';
            break;
          case Game.BLOCK:
            str += 'x';
            break;
          case Game.PADDLE:
            str += '_';
            break;
          case Game.BALL:
            str += 'o';
            break;
          default:
            break;
        }
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

  const game = new Game({
    x: 0, y: 0, intcode,
  });
  game.run();

  return game.getBlockTiles();
}

async function part2(input) {
  const intcode = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  // Play for free
  intcode[0] = 2;

  const game = new Game({
    x: 0, y: 0, intcode,
  });
  await game.runFreePlay();

  return game.score;
}

async function main() {
  const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
  console.log('day13 part1:', part1(input));
  console.log('day13 part2:', await part2(input));
}
main();
