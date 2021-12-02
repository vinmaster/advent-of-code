const fs = require('fs');
const path = require('path');

const COMMAND_REGEX = /^(\w+) (\d{1,})/;

const part1 = input => {
  const commands = input
    .trim()
    .split('\n')
    .map(s => s.match(COMMAND_REGEX));

  let x = 0;
  let y = 0;
  for (let [, dir, numStr] of commands) {
    switch (dir) {
      case 'forward':
        x += parseInt(numStr, 10);
        break;
      case 'down':
        y += parseInt(numStr, 10);
        break;
      case 'up':
        y -= parseInt(numStr, 10);
        break;
      default:
        break;
    }
  }
  return x * y;
};

const part2 = input => {
  const commands = input
    .trim()
    .split('\n')
    .map(s => s.match(COMMAND_REGEX));

  let x = 0;
  let y = 0;
  let aim = 0;
  for (let [, dir, numStr] of commands) {
    switch (dir) {
      case 'forward':
        let num = parseInt(numStr, 10);
        x += num;
        y += num * aim;
        break;
      case 'down':
        aim += parseInt(numStr, 10);
        break;
      case 'up':
        aim -= parseInt(numStr, 10);
        break;
      default:
        break;
    }
  }
  return x * y;
};

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
// input = `forward 5
// down 5
// forward 8
// up 3
// down 8
// forward 2`
console.log('day2 part1:', part1(input));
console.log('day2 part2:', part2(input));
