// deno-lint-ignore-file prefer-const

export function part1(input: string) {
  input = input.trim();
  let matches = [...input.matchAll(new RegExp('mul\\(\\d+,\\d+\\)', 'g'))].map(m => m[0]);
  return matches
    .map(m => {
      let [x, y] = m.match(/(\d+)/g)!.map(Number);
      return x * y;
    })
    .reduce((a, b) => a + b);
}

export function part2(input: string) {
  input = input.trim();
  let matches = [...input.matchAll(new RegExp(`mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)`, 'g'))].map(
    m => m[0]
  );
  let enabled = true;
  let list: string[] = [];
  for (let m of matches) {
    if (m === 'do()') {
      enabled = true;
      continue;
    } else if (m === `don't()`) enabled = false;

    if (enabled) list.push(m);
  }
  return list
    .map(m => {
      let [x, y] = m.match(/(\d+)/g)!.map(Number);
      return x * y;
    })
    .reduce((a, b) => a + b);
}

async function main(useRealInput = true) {
  let input =
    // @ts-ignore: next-line
    typeof Bun !== 'undefined'
      ? // @ts-ignore: next-line
        await Bun.file(`${import.meta.dir}/input.txt`).text()
      : typeof Deno !== 'undefined'
      ? await Deno.readTextFile(`${import.meta.dirname}/input.txt`)
      : '';

  //   let testInput = `
  // xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
  // `;
  let testInput = `
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
`;
  if (!useRealInput) input = testInput;

  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// main(false);
main();
