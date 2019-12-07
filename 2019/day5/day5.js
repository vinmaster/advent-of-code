const fs = require('fs');
const path = require('path');

function runProgram(program, inputValue) {
  let increment = 0;
  let outputCode = null;
  for (let i = 0; ; i += increment) {
    const opcodeData = program[i];
    if (opcodeData === 99) { break; }

    const input1 = program[i + 1];
    const input2 = program[i + 2];
    const output = program[i + 3];
    // opcodeData comes in as ABCDE

    // Get DE
    const opcode = opcodeData % 100;
    // Get C
    let param1Mode = Math.floor(opcodeData / 100) % 10;
    // Get B
    let param2Mode = Math.floor(opcodeData / 1000) % 10;
    // A is always in position mode. No code needed

    const param1 = param1Mode === 0 ? program[input1] : input1;
    const param2 = param2Mode === 0 ? program[input2] : input2;
 
    switch (opcode) {
      case 1:
        program[output] = param1 + param2;
        increment = 4;
        break;
      case 2:
        program[output] = param1 * param2;
        increment = 4;
        break;
      case 3:
        program[input1] = inputValue;
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
        if (param1 < param2) { program[output] = 1; }
        else { program[output] = 0; }
        increment = 4;
        break;
      case 8:
        if (param1 === param2) { program[output] = 1; }
        else { program[output] = 0; }
        increment = 4;
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

function part2(input, inputValue) {
  const program = input
    .trim()
    .split(',')
    .map(s => parseInt(s, 10));

  return runProgram(program, inputValue);
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day5 part1:', part1(input));
console.log('day5 part2:', part2(input, 5));

/*

let pc = 0
const input = 5
const output = []

while (memory[pc] != 99){ processOpcode() }

function processOpcode(){
  const opcode = memory[pc]
  const arg1 = memory[pc + 1], arg2 = memory[pc + 2], arg3 = memory[pc + 3]

  switch (opcode) {
    ///        #3rd param     #1st param     #2nd param
    case 1:    memory[arg3] = memory[arg1] +  memory[arg2];      pc += 4; break
    case 101:  memory[arg3] = arg1         +  memory[arg2];      pc += 4; break
    case 1001: memory[arg3] = memory[arg1] +  arg2;              pc += 4; break
    case 1101: memory[arg3] = arg1         +  arg2;              pc += 4; break

    case 2:    memory[arg3] = memory[arg1] *  memory[arg2];      pc += 4; break
    case 102:  memory[arg3] = arg1         *  memory[arg2];      pc += 4; break
    case 1002: memory[arg3] = memory[arg1] *  arg2;              pc += 4; break
    case 1102: memory[arg3] = arg1         *  arg2;              pc += 4; break

    case 7:    memory[arg3] = memory[arg1] <  memory[arg2];      pc += 4; break
    case 107:  memory[arg3] = arg1         <  memory[arg2];      pc += 4; break
    case 1007: memory[arg3] = memory[arg1] <  arg2;              pc += 4; break
    case 1107: memory[arg3] = arg1         <  arg2;              pc += 4; break

    case 8:    memory[arg3] = memory[arg1] == memory[arg2];      pc += 4; break
    case 108:  memory[arg3] = arg1         == memory[arg2];      pc += 4; break
    case 1008: memory[arg3] = memory[arg1] == arg2;              pc += 4; break
    case 1108: memory[arg3] = arg1         == arg2;              pc += 4; break

    ///         #condition      #Set ip to 2nd param
    case 105:  if (arg1 != 0) { pc = memory[arg2]; break }       pc += 3; break
    case 1005: if (memory[arg1] != 0) { pc = arg2; break }       pc += 3; break
    case 1105: if (arg1 != 0) { pc = arg2; break }               pc += 3; break
    case 106:  if (arg1 == 0) { pc = memory[arg2]; break }       pc += 3; break
    case 1006: if (memory[arg1] == 0) { pc = arg2; break }       pc += 3; break
    case 1106: if (arg1 == 0) { pc = arg2; break }               pc += 3; break

    ///        ## 1st-only param
    case 3:    memory[arg1] = input;                             pc += 2; break
    case 4:    output.push(memory[arg1]);                        pc += 2; break
    case 104:  output.push(arg1);                                pc += 2; break

    default: console.log("unimplemented opcode: " + opcode); process.exit()
  }
}

--------------------------------------------------------------------------------

const strs = require("fs").readFileSync("input.txt").toString().split(",")
const INPUT = 5

for (let ip = 0; ip < strs.length;) {
    const [i, as1, as2, as3] = strs.slice(ip)
    const [p1 = "0", p2 = "0"] = i.split("").reverse().slice(2)
    const a1 = +p1 ? +as1 : +strs[as1]
    const a2 = +p2 ? +as2 : +strs[as2]
    const a3 = +as3
    
    if (i == "99") {
        break
    } else if (i.endsWith("4")) {
        console.log(a1)
        ip += 2
    } else if (i === "3") {
        strs[+as1] = INPUT
        ip += 2
    } else if (i.endsWith("1")) {
        strs[a3] = String(a1 + a2)
        ip += 4
    } else if (i.endsWith("2")) {
        strs[a3] = String(a1 * a2)
        ip += 4
    } else if (i.endsWith("5")) {
        if (a1 != 0) ip = a2; 
        else ip += 3
    } else if (i.endsWith("6")) {
        if (a1 == 0) ip = a2; 
        else ip += 3
    } else if (i.endsWith("7")) {
        strs[a3] = +(a1 < a2)
        ip += 4
    } else if (i.endsWith("8")) {
        strs[a3] = +(a1 == a2)
        ip += 4
    } else {
        throw new Error(`Invalid OP Code '${i}'`)
    }
}

*/
