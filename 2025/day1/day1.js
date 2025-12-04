function part1(input) {
  let dial = 50;
  let result = 0;
  for (let line of input) {
    let [dir, numStr] = line.match(/(L|R)(\d+)/).slice(1);
    let num = Number(numStr);
    if (dir === 'L') {
      dial = (dial - num + 100) % 100;
    } else {
      dial = (dial + num) % 100;
    }
    if (dial === 0) result += 1;
  }
  console.log('part1:', result);
}

function part2(input) {
  let dial = 50;
  let result = 0;
  for (let line of input) {
    let [dir, numStr] = line.match(/(L|R)(\d+)/).slice(1);
    let num = Number(numStr);
    let step = dir === 'L' ? -1 : 1;
    for (let i = 0; i < num; i++) {
      dial += step;
      dial = (dial + 100) % 100;
      if (dial === 0) result += 1;
    }
  }
  console.log('part2:', result);
}

function parseInput(inputString) {
  let lines = inputString.trim().split('\n');
  return lines;
}

// Get input from browser
let inputString = $('*').innerText;
part1(parseInput(inputString));
part2(parseInput(inputString));
