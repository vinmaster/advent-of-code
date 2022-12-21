// deno-lint-ignore-file prefer-const no-explicit-any

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// root: pppw + sjmn
// dbpl: 5
// cczh: sllz + lgvd
// zczc: 2
// ptdq: humn - dvpt
// dvpt: 3
// lfqf: 4
// humn: 5
// ljgn: 2
// sjmn: drzm * dbpl
// sllz: 4
// pppw: cczh / lfqf
// lgvd: ljgn * ptdq
// drzm: hmdt - zczc
// hmdt: 32`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));

function part1(input: string) {
  let lines = input.trim().split('\n');
  let monkeys = {} as Record<string, string>;
  for (let line of lines) {
    let [name, math] = line.split(': ');
    monkeys[name] = math;
  }
  return wait(monkeys, 'root');
}

function part2(input: string) {
  let me = 'humn';
  let lines = input.trim().split('\n');
  let monkeys = {} as Record<string, string>;
  for (let line of lines) {
    let [name, math] = line.split(': ');
    monkeys[name] = math;
  }

  let [m1, _, m2] = monkeys['root'].split(' ');
  let path1 = getPath(monkeys, [], m1, me);
  let path2 = getPath(monkeys, [], m2, me);
  if (path1.length === 0) {
    return reachValue(monkeys, path2, wait(monkeys, m1));
  } else {
    return reachValue(monkeys, path1, wait(monkeys, m2));
  }
}

function wait(monkeys: Record<string, string>, name: string): number {
  let math = monkeys[name];
  if (math.length < 6) {
    return Number(math);
  } else {
    let [m1, op, m2] = math.split(' ');
    switch (op) {
      case '+':
        return wait(monkeys, m1) + wait(monkeys, m2);
      case '-':
        return wait(monkeys, m1) - wait(monkeys, m2);
      case '*':
        return wait(monkeys, m1) * wait(monkeys, m2);
      case '/':
        return wait(monkeys, m1) / wait(monkeys, m2);
      default:
        throw new Error('Op not found: ' + math);
    }
  }
}

function getPath(
  monkeys: Record<string, string>,
  path: string[],
  current: string,
  target: string
): string[] {
  if (current === target) {
    return [...path, current];
  }

  let math = monkeys[current];
  if (math.length < 6) {
    return [];
  } else {
    let [m1, _, m2] = math.split(' ');
    let path1 = getPath(monkeys, [...path, current], m1, target);
    let path2 = getPath(monkeys, [...path, current], m2, target);
    if (path1.length === 0) return path2;
    else return path1;
  }
}

function reachValue(monkeys: Record<string, string>, path: string[], target: number): number {
  let current = path.shift() || '';
  let [m1, op, m2] = (monkeys[current] || '').split(' ');

  switch (op) {
    case '+':
      if (m1 === path.at(0)) {
        return reachValue(monkeys, path, target - wait(monkeys, m2));
      } else {
        return reachValue(monkeys, path, target - wait(monkeys, m1));
      }
    case '-':
      if (m1 === path.at(0)) {
        return reachValue(monkeys, path, target + wait(monkeys, m2));
      } else {
        return reachValue(monkeys, path, wait(monkeys, m1) - target);
      }
    case '*':
      if (m1 === path.at(0)) {
        return reachValue(monkeys, path, target / wait(monkeys, m2));
      } else {
        return reachValue(monkeys, path, target / wait(monkeys, m1));
      }
    case '/':
      if (m1 === path.at(0)) {
        return reachValue(monkeys, path, target * wait(monkeys, m2));
      } else {
        return reachValue(monkeys, path, target / wait(monkeys, m1));
      }
    default:
      return target;
  }
}
