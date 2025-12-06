type Input = string[];

export function part1(input: Input) {
  let lines = input.slice(0, -1).map(line => line.match(/(\d)+/g)!.map(Number));
  let operators = input.at(-1)?.match(/(\S)/g)!;
  let answers = [] as number[];
  for (let problem = 0; problem < lines[0].length; problem++) {
    let answer = operators[problem] === '+' ? 0 : 1;
    for (let i = 0; i < lines.length; i++) {
      if (operators[problem] === '+') {
        answer += lines[i][problem];
      } else {
        answer *= lines[i][problem];
      }
    }
    answers.push(answer);
  }
  let sum = answers.reduce((x, sum) => x + sum);
  console.log('part1:', sum);
}

export function part2(input: Input) {
  let lines = input.slice(0, -1);
  let operatorLine = input.at(-1)!;
  // Get indexes of all the operators on that line
  let indexes = operatorLine
    .split('')
    .map((s, i) => (s !== ' ' ? i : null))
    .filter(x => x !== null);
  let widths = [] as number[];
  // Get widths of all the columns
  for (let i = 1; i < indexes.length; i++) {
    widths.push(indexes[i] - indexes[i - 1] - 1);
  }
  widths.push(operatorLine.length - indexes.at(-1)!);
  let answers = [] as number[];
  for (let problem = 0; problem < indexes.length; problem++) {
    let startIndex = indexes[problem];
    let operator = operatorLine[startIndex];
    let answer = operator === '+' ? 0 : 1;
    for (let col = startIndex; col < startIndex + widths[problem]; col++) {
      let numStr = '';
      for (let line = 0; line < lines.length; line++) {
        if (lines[line][col] === ' ') continue;
        numStr += lines[line][col];
      }
      if (operator === '+') {
        answer += Number(numStr);
      } else {
        answer *= Number(numStr);
      }
    }
    answers.push(answer);
  }
  let sum = answers.reduce((x, sum) => x + sum);
  console.log('part2:', sum);
}

function parseInput(inputString: string): Input {
  let lines = inputString.split('\n');
  if (lines.at(-1)?.length === 0) lines = lines.slice(0, -1);
  return lines;
}

async function main(useRealInput = true) {
  let inputString = '';
  try {
    // @ts-expect-error: next-line
    inputString = await Bun.file(`${import.meta.dir}/input.txt`).text();
  } catch (error) {
    useRealInput = false;
  }

  let testInput = `123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  `;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
  part2(parseInput(inputString));
}

// await main(false);
await main();
