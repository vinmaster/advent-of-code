function median(nums: number[]) {
  nums = [...nums].sort((a, b) => a - b);
  const middleIndex = Math.floor(nums.length / 2);
  return nums.length % 2 ? nums[middleIndex] : (nums[middleIndex - 1] + nums[middleIndex]) / 2;
}

function getIncomplete(str: string) {
  let open = { '(': ')', '[': ']', '{': '}', '<': '>' };
  let closed = { ')': true, ']': true, '}': true, '>': true };
  let stack: string[] = [];
  for (let cur of str.split('')) {
    if (open[cur]) {
      stack.push(cur);
    } else if (closed[cur]) {
      if (open[stack.pop()!] !== cur) {
        return cur;
      }
    }
  }
  return null;
}

function getCompleteList(str: string) {
  let open = { '(': ')', '[': ']', '{': '}', '<': '>' };
  let closed = { ')': true, ']': true, '}': true, '>': true };
  let stack: string[] = [];
  let complete: string[] = [];
  for (let cur of str.split('')) {
    if (open[cur]) {
      stack.push(cur);
    } else if (closed[cur]) {
      let next = stack.pop()!;
      if (open[next] !== cur) {
        complete.push(open[next]);
      }
    }
  }
  return stack.map(c => open[c]);
}

function part1(input: string) {
  let lines = input.trim().split('\n');
  let points = { ')': 3, ']': 57, '}': 1197, '>': 25137 };

  return lines
    .map(getIncomplete)
    .filter(str => !!str)
    .map(str => points[str!])
    .reduce((a, b) => a + b);
}

function part2(input: string) {
  let lines = input.trim().split('\n');
  let points = { ')': 1, ']': 2, '}': 3, '>': 4 };
  let scores: number[] = [];

  for (let line of lines) {
    let str = getIncomplete(line);
    if (str) continue;
    let list = getCompleteList(line);
    scores.push(list.reverse().reduce((acc, cur) => acc * 5 + points[cur], 0));
  }
  return median(scores);
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
