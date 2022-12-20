// deno-lint-ignore-file prefer-const no-explicit-any

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// 1
// 2
// -3
// 3
// -2
// 0
// 4`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));

function part1(input: string) {
  let numbers = input.trim().split('\n').map(Number);
  let order = new Array(numbers.length).fill(0).map((_, i) => i);
  let target = [1000, 2000, 3000];

  for (let i = 0; i < numbers.length; i++) {
    let index = order.indexOf(i);
    let num = numbers.splice(index, 1)[0];
    let newIndex = (index + num + numbers.length) % numbers.length;
    if (newIndex === 0) {
      newIndex = numbers.length;
    }
    order.splice(index, 1);
    numbers.splice(newIndex, 0, num);
    order.splice(newIndex, 0, i);
  }

  let targetIndex = numbers.indexOf(0);
  let res = target.map(i => numbers.at((targetIndex + i) % numbers.length)) as number[];
  return res.reduce((a, b) => a + b);
}

function part2(input: string) {
  let numbers = input.trim().split('\n').map(Number);
  numbers = numbers.map(n => n * 811589153);
  let order = new Array(numbers.length).fill(0).map((_, i) => i);
  let target = [1000, 2000, 3000];

  for (let mix = 0; mix < 10; mix++) {
    for (let i = 0; i < numbers.length; i++) {
      let index = order.indexOf(i);
      let num = numbers.splice(index, 1)[0];
      let newIndex = (index + num + numbers.length) % numbers.length;
      if (newIndex === 0) {
        newIndex = numbers.length;
      }
      order.splice(index, 1);
      numbers.splice(newIndex, 0, num);
      order.splice(newIndex, 0, i);
    }
    // console.log(numbers);
  }

  let targetIndex = numbers.indexOf(0);
  let res = target.map(i => numbers.at((targetIndex + i) % numbers.length)) as number[];
  return res.reduce((a, b) => a + b);
}
