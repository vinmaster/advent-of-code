const fs = require('fs');
const path = require('path');

function part1(input) {
  /** @type string[] */
  let instructions = input.trim().split('\n');
  let x = 1;
  let cycle = 1;
  let runCycles = 0;
  let instr = '';

  let check = [20, 60, 100, 140, 180, 220];
  let sum = 0;

  while (true) {
    if (runCycles <= 0) {
      if (instr.includes('addx')) {
        let [_, s] = instr.split(' ');
        x += parseInt(s, 10);
      }
      instr = instructions.shift();
      if (instr === undefined) break;

      if (instr.includes('noop')) {
        runCycles = 1;
      } else if (instr.includes('addx')) {
        runCycles = 2;
      }
    }

    if (check.includes(cycle)) {
      sum += cycle * x;
    }

    runCycles -= 1;
    cycle += 1;
  }
  return sum;
}

function part2(input) {
  /** @type string[] */
  let instructions = input.trim().split('\n');
  let x = 1;
  let cycle = 1;
  let runCycles = 0;
  let instr = '';

  let screen = '\n';
  let screenWidth = 40;
  let visible = (cycle, x) => {
    let c = cycle % 40;
    let range = c - x;
    return range <= 2 && range >= 0;
  };

  while (true) {
    if (runCycles <= 0) {
      if (instr.includes('addx')) {
        let [_, s] = instr.split(' ');
        x += parseInt(s, 10);
      }
      instr = instructions.shift();
      if (instr === undefined) break;

      if (instr.includes('noop')) {
        runCycles = 1;
      } else if (instr.includes('addx')) {
        runCycles = 2;
      }
    }

    visible(cycle, x) ? (screen += '#') : (screen += '.');
    if (cycle % screenWidth === 0) {
      screen += '\n';
    }

    runCycles -= 1;
    cycle += 1;
  }
  // console.log(screen);
  return screen;
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8').replaceAll('\r', '');

// input = `
// addx 15
// addx -11
// addx 6
// addx -3
// addx 5
// addx -1
// addx -8
// addx 13
// addx 4
// noop
// addx -1
// addx 5
// addx -1
// addx 5
// addx -1
// addx 5
// addx -1
// addx 5
// addx -1
// addx -35
// addx 1
// addx 24
// addx -19
// addx 1
// addx 16
// addx -11
// noop
// noop
// addx 21
// addx -15
// noop
// noop
// addx -3
// addx 9
// addx 1
// addx -3
// addx 8
// addx 1
// addx 5
// noop
// noop
// noop
// noop
// noop
// addx -36
// noop
// addx 1
// addx 7
// noop
// noop
// noop
// addx 2
// addx 6
// noop
// noop
// noop
// noop
// noop
// addx 1
// noop
// noop
// addx 7
// addx 1
// noop
// addx -13
// addx 13
// addx 7
// noop
// addx 1
// addx -33
// noop
// noop
// noop
// addx 2
// noop
// noop
// noop
// addx 8
// noop
// addx -1
// addx 2
// addx 1
// noop
// addx 17
// addx -9
// addx 1
// addx 1
// addx -3
// addx 11
// noop
// noop
// addx 1
// noop
// addx 1
// noop
// noop
// addx -13
// addx -19
// addx 1
// addx 3
// addx 26
// addx -30
// addx 12
// addx -1
// addx 3
// addx 1
// noop
// noop
// noop
// addx -9
// addx 18
// addx 1
// addx 2
// noop
// noop
// addx 9
// noop
// noop
// noop
// addx -1
// addx 2
// addx -37
// addx 1
// addx 3
// noop
// addx 15
// addx -21
// addx 22
// addx -6
// addx 1
// noop
// addx 2
// addx 1
// noop
// addx -10
// noop
// noop
// addx 20
// addx 1
// addx 2
// addx 2
// addx -6
// addx -11
// noop
// noop
// noop`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
