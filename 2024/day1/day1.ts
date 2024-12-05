export function part1(input: string) {
  input = input.trim();
  let list1: number[] = [];
  let list2: number[] = [];
  for (let line of input.split('\n')) {
    let [n1, n2] = line.split('   ').map(Number);
    list1.push(n1);
    list2.push(n2);
  }
  list1 = list1.sort((a, b) => a - b);
  list2 = list2.sort((a, b) => a - b);

  let sum = 0;
  for (let i = 0; i < list1.length; i++) {
    let distance = Math.abs(list1[i] - list2[i]);
    sum += distance;
  }
  return sum;
}

export function part2(input: string) {
  input = input.trim();
  let list1: number[] = [];
  let list2: number[] = [];
  for (let line of input.split('\n')) {
    let [n1, n2] = line.split('   ').map(Number);
    list1.push(n1);
    list2.push(n2);
  }

  let sum = 0;
  for (let i = 0; i < list1.length; i++) {
    let score = list1[i] * list2.filter(x => x === list1[i]).length;
    sum += score;
  }
  return sum;
}

async function main(useRealInput = true) {
  let input =
    // @ts-expect-error: next-line
    typeof Bun !== 'undefined'
      ? // @ts-expect-error: next-line
        await Bun.file(`${import.meta.dir}/input.txt`).text()
      : // @ts-expect-error: next-line
      typeof Deno !== 'undefined'
      ? // @ts-expect-error: next-line
        await Deno.readTextFile(`${import.meta.dirname}/input.txt`)
      : '';

  let testInput = `
3   4
4   3
2   5
1   3
3   9
3   3
`;
  if (!useRealInput) input = testInput;

  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// main(false);
// main();
