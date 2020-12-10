const { timeStamp } = require('console');
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

class Droid {
  static TILES = {
    DROID: 'D',
    WALL: '#',
    GROUND: ' ',
    OXYGEN_SYSTEM: '!',
    UNKNOWN: '?',
  };
  static DIR = {
    NORTH: 1,
    SOUTH: 2,
    WEST: 3,
    EAST: 4,
  };
  // offset = dx, dy
  static OFFSET = {
    [this.DIR.NORTH]: { x: 0, y: -1 },
    [this.DIR.SOUTH]: { x: 0, y: 1 },
    [this.DIR.WEST]: { x: -1, y: 0 },
    [this.DIR.EAST]: { x: 1, y: 0 },
  }

  constructor({ intcode }) {
    this.program = new Program(intcode);
    this.x = 0;
    this.y = 0;
    this.step = 0;
    this.visited = { [this.coord]: 0 };
    this.tiles = { [this.coord]: Droid.TILES.DROID };
    this.facing = Droid.DIR.NORTH;
    this.oxygenSystemCoord = null;
  }

  get direction() {
    return Object.entries(Droid.DIR).filter(([name, num]) => num === this.facing)[0][0];
  }

  get coord() {
    return `${this.x},${this.y}`;
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
    return str;
  }

  setNextMove() {

  }

  turnRight() {
    if (this.facing === Droid.DIR.NORTH) this.facing = Droid.DIR.EAST;
    else if (this.facing === Droid.DIR.EAST) this.facing = Droid.DIR.SOUTH;
    else if (this.facing === Droid.DIR.SOUTH) this.facing = Droid.DIR.WEST;
    else if (this.facing === Droid.DIR.WEST) this.facing = Droid.DIR.NORTH;
  }

  turnLeft() {
    if (this.facing === Droid.DIR.NORTH) this.facing = Droid.DIR.WEST;
    else if (this.facing === Droid.DIR.WEST) this.facing = Droid.DIR.SOUTH;
    else if (this.facing === Droid.DIR.SOUTH) this.facing = Droid.DIR.EAST;
    else if (this.facing === Droid.DIR.EAST) this.facing = Droid.DIR.NORTH;
  }

  move() {
    let input = this.facing;
    this.program.run([input]);
    if (this.program.outputs.length > 0) {
      return this.program.outputs.shift();
    } else {
      throw new Error('No new output');
    }
  }

  findOxygenSystem() {
    // Try to follow right hand rule
    // Alternative is to turn left when wall and turn right when no wall
    let checkingRight = true;

    for (let i = 0; i < 10000; i++) {
      if (checkingRight) {
        this.turnRight();
      }

      let output = this.move();
      let { x, y } = Droid.OFFSET[this.facing];
      if (output === 0) {
        // Hit a wall
        this.tiles[`${this.x + x},${this.y + y}`] = Droid.TILES.WALL;
        this.turnLeft();
        checkingRight = false;
      } else if (output === 1) {
        // Moved there
        this.tiles[this.coord] = Droid.TILES.GROUND;
        this.x += x;
        this.y += y;
        this.tiles[this.coord] = Droid.TILES.DROID;
        this.step++;
        if (this.visited[this.coord] !== undefined) {
          this.step = this.visited[this.coord];
        } else {
          this.visited[this.coord] = this.step;
        }
        checkingRight = true;
      } else if (output === 2) {
        // Moved there
        this.tiles[this.coord] = Droid.TILES.GROUND;
        this.x += x;
        this.y += y;
        this.step++;
        if (this.visited[this.coord] !== undefined) {
          this.step = this.visited[this.coord];
        } else {
          this.visited[this.coord] = this.step;
        }
        this.tiles[this.coord] = Droid.TILES.DROID;
        this.oxygenSystemCoord = this.coord;
      }
    }
    return this.oxygenSystemCoord;
  }

  spreadOxygen() {
    let getXY = (coord) => {
      let [x, y] = coord.split(',');
      return { x: parseInt(x, 10), y: parseInt(y, 10) };
    };
    let getAdjacent = ({ x, y }) => {
      // Get up, down, left, right
      let dv = [{ dx: 0, dy: -1 }, { dx: 0, dy: 1 }, { dx: -1, dy: 0 }, { dx: 1, dy: 0 }]
      return dv
        .map(({ dx, dy }) => ({ x: x + dx, y: y + dy }))
        .filter(({ x, y }) => !['#', 'O'].includes(this.tiles[`${x},${y}`]));
    }


    let minutes = 0;
    let queue = [[getXY(this.oxygenSystemCoord)]];
    while (queue.length !== 0) {
      let currentList = queue.shift();
      currentList.map(n => this.tiles[`${n.x},${n.y}`] = 'O');
      let nextList = currentList.map(c => getAdjacent(c)).flat();
      if (nextList.length !== 0) queue.push(nextList);
      minutes++;
    }
    return minutes - 1;
  }
}

function part1(input) {
  const intcode = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  const droid = new Droid({ intcode });
  let coord = droid.findOxygenSystem();
  return droid.visited[droid.oxygenSystemCoord];
}

function part2(input) {
  const intcode = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  const droid = new Droid({ intcode });
  let coord = droid.findOxygenSystem();
  let minutes = droid.spreadOxygen();
  // console.log(droid.getDisplay());
  return minutes;
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

console.log('day15 part1:', part1(input));
console.log('day15 part2:', part2(input));
