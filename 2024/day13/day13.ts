type Machine = { a: number[]; b: number[]; prize: number[] };
type Input = Machine[];

function addCoord(a, b) {
  return [a[0] + b[0], a[1] + b[1]];
}

// function findFewestTokens(
//   machine: Machine,
//   current = [0, 0],
//   tokens = Number.MAX_SAFE_INTEGER,
//   count = {} as Record<string, number>
// ) {
//   let tokenACost = 3;
//   let tokenBCost = 1;
//   count['a'] ??= 0;
//   count['b'] ??= 0;
//   // Check if at prize or over the limit
//   if (current[0] === machine.prize[0] && current[1] === machine.prize[1]) return tokens;
//   if (
//     current[0] > machine.prize[0] ||
//     current[1] > machine.prize[1] ||
//     Object.values(count).every(num => num > 100)
//   )
//     return Number.MAX_SAFE_INTEGER;
//   // Try each buttons
//   let tryA = findFewestTokens(machine, addCoord(current, machine.a), tokens + tokenACost, {
//     ...count,
//     a: count.a + 1,
//   });
//   let tryB = findFewestTokens(machine, addCoord(current, machine.b), tokens + tokenBCost, {
//     ...count,
//     b: count.b + 1,
//   });
//   return Math.min(tryA, tryB);
// }

/*
Using Cramer's rule: https://byjus.com/maths/cramers-rule/
a1x + b1y = c1
a2x + b2y = c2
2 equations becomes: AX = B
D = |A| = a1b2 - a2b1
Dx = c1b2 - c2b1
Dy = a1c2 - a2c1
x = Dx/D
y = Dy/D
*/
function solveEquation(eq1, eq2) {
  let [a1, b1, c1] = eq1;
  let [a2, b2, c2] = eq2;
  let d = a1 * b2 - a2 * b1;
  let dx = c1 * b2 - c2 * b1;
  let dy = a1 * c2 - a2 * c1;
  let x = dx / d;
  let y = dy / d;
  return [x, y];
}

function findFewestTokens(machine: Machine) {
  let [a, b] = solveEquation(
    [machine.a[0], machine.b[0], machine.prize[0]],
    [machine.a[1], machine.b[1], machine.prize[1]]
  );
  return a * 3 + b;
}

export function part1(input: Input) {
  let total = 0;
  for (let machine of input) {
    let tokens = findFewestTokens(machine);
    if (Number.isInteger(tokens)) total += tokens;
  }
  return total;
}

export function part2(input: Input) {
  let correction = 10000000000000;
  input.forEach(machine => {
    machine.prize[0] += correction;
    machine.prize[1] += correction;
  });
  return part1(input);
}

function parseInput(inputString: string): Input {
  return inputString
    .trim()
    .split('\n\n')
    .map(machineData => {
      let [aData, bData, prizeData] = machineData.split('\n');
      let a = aData
        .match(/.*X(.\d+), Y(.\d+).*/)!
        .slice(1)
        .map(Number);
      let b = bData
        .match(/.*X(.\d+), Y(.\d+).*/)!
        .slice(1)
        .map(Number);
      let prize = prizeData
        .match(/.*X=(.\d+), Y=(.\d+).*/)!
        .slice(1)
        .map(Number);
      return { a, b, prize };
    });
}

async function main(useRealInput = true) {
  let inputString = '';
  try {
    inputString =
      // @ts-expect-error: next-line
      typeof Bun !== 'undefined'
        ? // @ts-expect-error: next-line
          await Bun.file(`${import.meta.dir}/input.txt`).text()
        : // @ts-expect-error: next-line
        typeof Deno !== 'undefined'
        ? // @ts-expect-error: next-line
          await Deno.readTextFile(`${import.meta.dirname}/input.txt`)
        : '';
  } catch (error) {
    useRealInput = false;
  }

  let testInput = `
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
`;

  if (!useRealInput) inputString = testInput;

  let input = parseInput(inputString);
  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// await main(false);
await main();
