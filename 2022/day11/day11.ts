// deno-lint-ignore-file prefer-const

function parseInput(lines: string[]) {
  let monkeys = [];
  for (let line of lines) {
    let [_, items, op, test, ifTrue, ifFalse] = line.split('\n');
    items = items.split('Starting items: ')[1];
    op = op.split('Operation: new = old ')[1];
    test = test.split('Test: divisible by ')[1];
    ifTrue = ifTrue.split('If true: throw to monkey ')[1];
    ifFalse = ifFalse.split('If false: throw to monkey ')[1];

    monkeys.push({
      items: items.split(', ').map(s => parseInt(s, 10)),
      // newItems: [] as number[],
      op: (num: number) => {
        if (op === '* old') {
          return num * num;
        } else if (op[0] === '*') {
          return num * parseInt(op.split(' ')[1], 10);
        } else if (op[0] === '+') {
          return num + parseInt(op.split(' ')[1], 10);
        } else throw new Error(`Invalid operation: ${op}`);
      },
      test: parseInt(test, 10),
      ifTrue: parseInt(ifTrue, 10),
      ifFalse: parseInt(ifFalse, 10),
      inspect: 0,
    });
  }
  return monkeys;
}

function greatestCommonDivisor(a: number, b: number): number {
  let remainder = 0;
  if (a === 0) {
    return b;
  } else if (b === 0) {
    return a;
  } else {
    while (b !== 0) {
      remainder = a % b;
      a = b;
      b = remainder;
    }
    return a;
  }
}

function leastCommonMultiple(nums: number[]): number {
  let least = 1;
  for (let num of nums) {
    least = Math.floor((least * num) / greatestCommonDivisor(least, num));
  }
  return least;
}

function part1(input: string) {
  let lines = input.trim().split('\n\n');
  let monkeys = parseInput(lines);

  for (let i = 0; i < 20; i++) {
    for (let monkey of monkeys) {
      for (let j = 0; j < monkey.items.length; ) {
        let item = monkey.items[j];
        let newItem = Math.floor(monkey.op(item) / 3);
        monkey.items.shift();
        monkey.inspect += 1;
        if (newItem % monkey.test === 0) {
          monkeys[monkey.ifTrue].items.push(newItem);
        } else {
          monkeys[monkey.ifFalse].items.push(newItem);
        }
      }
    }
  }

  let inspects = monkeys.map(m => m.inspect);
  inspects.sort((a, b) => b - a);

  return inspects[0] * inspects[1];
}

function part2(input: string) {
  let lines = input.trim().split('\n\n');
  let monkeys = parseInput(lines);
  let mod = leastCommonMultiple(monkeys.flatMap(m => m.test));

  for (let round = 0; round < 10_000; round++) {
    for (let monkey of monkeys) {
      for (let j = 0; j < monkey.items.length; ) {
        let item = monkey.items[j];
        let newItem = monkey.op(item) % mod;
        monkey.items.shift();
        monkey.inspect += 1;
        if (newItem % monkey.test === 0) {
          monkeys[monkey.ifTrue].items.push(newItem);
        } else {
          monkeys[monkey.ifFalse].items.push(newItem);
        }
      }
    }
  }

  let inspects = monkeys.map(m => m.inspect);
  inspects.sort((a, b) => b - a);

  return inspects[0] * inspects[1];
}

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// Monkey 0:
//   Starting items: 79, 98
//   Operation: new = old * 19
//   Test: divisible by 23
//     If true: throw to monkey 2
//     If false: throw to monkey 3

// Monkey 1:
//   Starting items: 54, 65, 75, 74
//   Operation: new = old + 6
//   Test: divisible by 19
//     If true: throw to monkey 2
//     If false: throw to monkey 0

// Monkey 2:
//   Starting items: 79, 60, 97
//   Operation: new = old * old
//   Test: divisible by 13
//     If true: throw to monkey 1
//     If false: throw to monkey 3

// Monkey 3:
//   Starting items: 74
//   Operation: new = old + 3
//   Test: divisible by 17
//     If true: throw to monkey 0
//     If false: throw to monkey 1`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
