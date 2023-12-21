function part1(input: string) {
  let [workflowsStr, partsStr] = input.trim().split('\n\n');
  let workflowsLines = workflowsStr.split('\n');
  let parts = partsStr.split('\n');

  let workflows: Record<string, string[]> = {};
  for (let line of workflowsLines) {
    let [name, rulesStr] = /^(\w+)\{(.+)\}$/.exec(line)!.slice(1);
    let rules = rulesStr.split(',');
    workflows[name] = rules;
  }
  let sum = 0;
  for (let line of parts) {
    let current = 'in';
    let [x, m, a, s] = line.match(/\d+/g)!.map(Number);
    let isDone = false;
    while (!isDone) {
      let rules = workflows[current];
      let vars = `let x=${x},m=${m},a=${a},s=${s}`;
      for (let rule of rules) {
        if (rule === 'A') {
          sum += x + m + a + s;
          isDone = true;
          break;
        } else if (rule === 'R') {
          isDone = true;
          break;
        } else if (rule.includes(':')) {
          let [conditional, nextName] = rule.split(':');
          if (eval(`${vars};${conditional}`)) {
            if (nextName === 'A') {
              sum += x + m + a + s;
              isDone = true;
              break;
            } else if (nextName === 'R') {
              isDone = true;
              break;
            }
            current = nextName;
            break;
          } else {
          }
        } else {
          current = rule;
        }
      }
    }
  }
  return sum;
}

function part2(input: string) {
  let [workflowsStr, partsStr] = input.trim().split('\n\n');
  let workflowsLines = workflowsStr.split('\n');
  let parts = partsStr.split('\n');

  let workflows: Record<string, any[]> = {};
  for (let line of workflowsLines) {
    let [name, rulesStr] = /^(\w+)\{(.+)\}$/.exec(line)!.slice(1);
    let rules = rulesStr.split(',');
    workflows[name] = [[], rules.pop()];
    for (let rule of rules) {
      let [pred, target] = rule.split(':');
      let [key, cmp, n] = [pred[0], pred[1], Number(pred.slice(2))];
      workflows[name][0].push([key, cmp, n, target]);
    }
  }
  let countRanges = (ranges: Record<string, [number, number]>, name = 'in'): number => {
    if (name === 'R') return 0;
    else if (name === 'A')
      return Object.values(ranges).reduce(
        (product, [start, end]) => (product *= end - start + 1),
        1
      );
    let [rules, fallback] = workflows[name];
    // console.log(rules, fallback);
    let total = 0;
    let hasLeftover = true;
    for (let [key, cmp, n, target] of rules) {
      let [start, end] = ranges[key];
      let truePred, falsePred;
      if (cmp === '<') {
        truePred = [start, Math.min(n - 1, end)];
        falsePred = [Math.max(n, start), end];
      } else {
        truePred = [Math.max(n + 1, start), end];
        falsePred = [start, Math.min(n, end)];
      }
      if (truePred[0] <= truePred[1]) {
        let copy = JSON.parse(JSON.stringify(ranges));
        copy[key] = truePred;
        total += countRanges(copy, target);
      }
      if (falsePred[0] <= falsePred[1]) {
        ranges = JSON.parse(JSON.stringify(ranges));
        ranges[key] = falsePred;
      } else {
        hasLeftover = false;
        break;
      }
    }
    if (hasLeftover) total += countRanges(ranges, fallback);
    return total;
  };
  let ranges = 'xmas'.split('').reduce((acc, s) => {
    acc[s] = [1, 4000];
    return acc;
  }, {});
  return countRanges(ranges);
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
