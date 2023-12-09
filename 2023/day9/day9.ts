function part1(input: string) {
  const lines = input.trim().split('\n');
  let sum = 0;

  for (let line of lines) {
    let nums = line.split(' ').map(Number);
    let arrays = [[...nums]];
    for (let i = 0; !(arrays.at(-1) as number[]).every(n => n === 0); i++) {
      arrays.push([]);
      let current = arrays[i];
      for (let j = 0; j < current.length - 1; j++) {
        arrays[i + 1].push(current[j + 1] - current[j]);
      }
    }
    (arrays.at(-1) as number[]).push(0);
    for (let i = arrays.length - 1; i > 0; i--) {
      arrays[i - 1].push(arrays[i - 1].at(-1) + arrays[i].at(-1));
    }
    sum += arrays[0].at(-1);
  }
  return sum;
}

function part2(input: string) {
  const lines = input.trim().split('\n');
  let sum = 0;

  for (let line of lines) {
    let nums = line.split(' ').map(Number);
    let arrays = [[...nums]];
    for (let i = 0; !(arrays.at(-1) as number[]).every(n => n === 0); i++) {
      arrays.push([]);
      let current = arrays[i];
      for (let j = 0; j < current.length - 1; j++) {
        arrays[i + 1].push(current[j + 1] - current[j]);
      }
    }
    (arrays.at(-1) as number[]).unshift(0);
    for (let i = arrays.length - 1; i > 0; i--) {
      arrays[i - 1].unshift(arrays[i - 1][0] - arrays[i][0]);
    }
    sum += arrays[0][0];
  }
  return sum;
}

let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
