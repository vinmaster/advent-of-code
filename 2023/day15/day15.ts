function part1(input: string) {
  let lines = input.trim().split(',');

  let sum = 0;
  for (let line of lines) {
    let value = 0;
    for (let c of line.split('')) {
      value += c.charCodeAt(0);
      value *= 17;
      value %= 256;
    }
    sum += value;
  }
  return sum;
}

function part2(input: string) {
  let lines = input.trim().split(',');
  let boxes = new Array(256).fill(0).map(() => new Array());

  for (let line of lines) {
    let value = 0;
    if (line.includes('=')) {
      let [label, num] = line.split('=');
      for (let c of label) {
        value += c.charCodeAt(0);
        value *= 17;
        value %= 256;
      }
      // boxes[value][label] ??= [];
      let found = false;
      for (let lens of boxes[value]) {
        if (lens[0] === label) {
          lens[1] = Number(num);
          found = true;
        }
      }
      if (!found) boxes[value].push([label, Number(num)]);
    } else if (line.includes('-')) {
      let [label] = line.split('-');
      for (let box of boxes) {
        for (let i = 0; i < box.length; ) {
          if (box[i][0] === label) {
            box.splice(i, 1);
          } else {
            i += 1;
          }
        }
      }
    }
  }
  let sum = 0;
  for (let box = 1; box <= boxes.length; box++) {
    let b = boxes[box - 1];
    if (b.length === 0) continue;
    sum += b.map(([_, focalLength], i) => box * focalLength * (i + 1)).reduce((a, b) => a + b);
  }
  return sum;
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
