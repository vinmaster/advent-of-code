const fs = require('fs');
const path = require('path');

const log = (...args) => console.log(...args);
const occurences = (str, target) => (str.match(new RegExp(target, 'g')) || []).length
const replaceAll = (str, search, replaceWith) => str.replace(new RegExp(search, 'g'), replaceWith);
const hasNumber = (str) => /\d/.test(str);
const toAscii = (str) => str.split('').map(c => c.charCodeAt());

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

class ASCII {
  static TILES = {
    SCAFFOLD: '#',
    SPACE: '.',
  };

  constructor({ intcode }) {
    this.program = new Program([...intcode]);
    this.display = '';
    this.outputs = [];
    this.tiles = {};
  }

  runCamera() {
    while (!this.program.exit) {
      this.program.run([]);
      if (this.program.outputs.length > 0) {
        let output = this.program.outputs.shift();
        this.display += String.fromCharCode(output);
      }
    }
  }

  printTiles() {
    let coords = Object.keys(this.tiles)
      .map(s => s.split(',').map(pos => parseInt(pos, 10)));
    let xs = coords.map(([x, y]) => x);
    let ys = coords.map(([x, y]) => y);
    let maxX = Math.max(...xs);
    let minX = Math.min(...xs);
    let maxY = Math.max(...ys);
    let minY = Math.min(...ys);

    let str = '';
    for (let row = minY; row <= maxY; row++) {
      for (let col = minX; col <= maxX; col++) {
        let key = `${col},${row}`;
        let s = this.tiles[key];
        if (key === this.oxygenSystemCoord) s = 'O';
        str += s ? s : Droid.TILES.UNKNOWN;
      }
      str += '\n';
    }
    log(str);
    return str;
  }

  scanDisplay() {
    let lines = this.display.split('\n');

    for (let y = 0; y < lines.length; y++) {
      for (let x = 0; x < lines[y].length; x++) {
        this.tiles[`${x},${y}`] = lines[y][x];
      }
    }
  }

  isIntersection(coord) {
    // Up, down, left, right
    let directions = [
      { x: 0, y: -1 },
      { x: 0, y: 1 },
      { x: -1, y: 0 },
      { x: 1, y: 0 },
    ];
    let [x, y] = coord.split(',').map(Number);
    return this.tiles[`${x},${y}`] === '#' && directions.every(d => this.tiles[`${x + d.x},${y + d.y}`] === '#');
  }

  getIntersections() {
    return Object.keys(this.tiles).filter(coord => this.isIntersection(coord));
  }

  coord(x, y) {
    return `${x},${y}`;
  }

  tilesAt(x, y) {
    return this.tiles[this.coord(x, y)];
  }

  calculatePath() {
    let starting = Object.keys(this.tiles).filter(coord => !['#', '.'].includes(this.tiles[coord]))[0];
    let [x, y] = starting.split(',').map(Number);
    let facingDirs = ['^', '>', 'v', '<'];
    let dv = [[0, -1], [1, 0], [0, 1], [-1, 0]];
    let facing = this.tilesAt(x, y);
    let instructions = [];

    while (true) {
      let currentIndex = facingDirs.indexOf(facing);
      let behind = (currentIndex + 2) % facingDirs.length;
      let open = dv.filter(([dx, dy], i) => i !== behind && this.tilesAt(x + dx, y + dy) === '#')
      if (open.length === 0) {
        break;
      }
      let nextIndex = dv.indexOf(open[0]);
      if ((currentIndex + 1) % facingDirs.length === nextIndex) {
        instructions.push('R');
      } else {
        instructions.push('L');
      }
      facing = facingDirs[nextIndex];
      currentIndex = facingDirs.indexOf(facing)
      this.tiles[this.coord(x, y)] = facing;
      // Move till hit the wall
      let [dx, dy] = open[0];
      let count = 0;
      while (this.tilesAt(x + dx, y + dy) === '#') {
        this.tiles[this.coord(x, y)] = '#';
        x += dx;
        y += dy;
        this.tiles[this.coord(x, y)] = facing;
        count++;
      }
      instructions.push(count.toString());
    }

    return instructions;
  }

  groupInstructions(instructions) {
    // Max length of instructions in main routine and movement functions
    let maxChars = 20;
    let notFnInst = s => !['A', 'B', 'C'].includes(s);

    let fn1 = [];
    let fn2 = [];
    let fn3 = [];
    let fn1Start = 0;

    for (let fn1End = fn1Start + 1; fn1End < instructions.length; fn1End++) {
      fn1 = instructions.slice(fn1Start, fn1End);
      let fn1Str = fn1.join(',');
      let rest1 = replaceAll(instructions.join(','), fn1Str, 'A').split(',');
      let fn2Start = rest1.findIndex(notFnInst);

      for (let fn2End = fn2Start + 1; fn2End < rest1.length; fn2End++) {
        fn2 = rest1.slice(fn2Start, fn2End);
        if (fn2.length > maxChars) continue;
        let fn2Str = fn2.join(',');
        let rest2 = replaceAll(rest1.join(','), fn2Str, 'B').split(',');
        let fn3Start = rest2.findIndex(notFnInst);

        for (let fn3End = fn3Start + 1; fn3End < rest2.length; fn3End++) {
          fn3 = rest2.slice(fn3Start, fn3End);
          if (fn3.length > maxChars) continue;
          let fn3Str = fn3.join(',');
          let mainRoutine = replaceAll(rest2.join(','), fn3Str, 'C').split(',');

          // Found solution
          if (!hasNumber(mainRoutine)) {
            return [mainRoutine, fn1, fn2, fn3];
          }
        }
      }
    }

    // No solution
    return null;
  }

  runVacuum(inputs) {
    while (!this.program.exit) {
      this.program.run(inputs);
      if (this.program.outputs.length > 0) {
        let output = this.program.outputs.shift();
        this.outputs.push(output);
      }
    }
  }
}

function part1(input) {
  const intcode = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  const ascii = new ASCII({ intcode });
  ascii.runCamera();
  ascii.scanDisplay();
  let intersections = ascii.getIntersections();
  return intersections.map(coord => {
    let [x, y] = coord.split(',').map(Number);
    return x * y;
  }).reduce((sum, n) => sum + n);
}

function part2(input) {
  const intcode = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  let ascii = new ASCII({ intcode });
  ascii.runCamera();
  ascii.scanDisplay();
  let instructions = ascii.calculatePath();
  let solution = ascii.groupInstructions(instructions);
  // ascii.printTiles();
  solution = solution ?? [];

  let inputs = [];
  for (let fn of solution) {
    let asciis = fn.map(c => toAscii(c));
    // Add commas inbetween
    asciis = asciis.join(',' + ','.charCodeAt() + ',').split(',');
    inputs = inputs.concat(asciis);
    // Add newline after each fn
    inputs.push('\n'.charCodeAt().toString());
  }
  inputs = inputs.map(Number);
  inputs.push('n'.charCodeAt(), '\n'.charCodeAt());
  intcode[0] = 2;
  ascii = new ASCII({ intcode });
  // log(inputs.map(i => String.fromCharCode(i)).join(''));

  ascii.runVacuum(inputs);

  return ascii.outputs.pop();
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day17 part1:', part1(input));
log('day17 part2:', part2(input));
