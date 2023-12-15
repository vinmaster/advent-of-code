const CACHE = new Map();

function memoize<Args extends unknown[], Result>(
  fn: (...args: Args) => Result,
  ...args: Args
): Result {
  let key = `${fn.name}(${JSON.stringify(args)})`;
  if (CACHE.has(key)) {
    return CACHE.get(key);
  } else {
    // console.count(key);
    let result = fn(...args);
    CACHE.set(key, result);
    return result;
  }
}

function memoizeFn<Args extends unknown[], Result>(
  func: (...args: Args) => Result
): (...args: Args) => Result {
  const cache = new Map<string, Result>();
  return (...args) => {
    const k = JSON.stringify(args);
    if (cache.has(k)) {
      return cache.get(k)!;
    }
    const result = func(...args);
    cache.set(k, result);
    return result;
  };
}

function getPermutations(chars: string[], length: number) {
  let result: string[] = [];
  let stack: string[] = [''];
  while (stack.length !== 0) {
    let word = stack.shift();
    for (let c of chars) {
      let newWord = word + c;
      if (newWord.length === length) {
        result.push(newWord);
      } else {
        stack.push(newWord);
      }
    }
  }
  return result;
}

function getPermutationsMemoize(chars: string[], length: number) {
  const permutate = (chars: string[], length: number) => {
    if (length === 0) return [''];
    let result: string[] = [];
    for (let c of chars) {
      for (let subword of memoize(permutate, chars, length - 1)) {
        result.push(c + subword);
      }
    }
    return result;
  };
  return memoize(permutate, chars, length);
}

function getPermutationsMemoize2(chars: string[], length: number) {
  const permutate = (chars: string[], length: number, current: string = '') => {
    if (current.length === length) return [current];
    let result: string[] = [];
    for (let c of chars) {
      let newWord = current + c;
      result = result.concat(memoize(permutate, chars, length, newWord));
    }
    return result;
  };
  return memoize(permutate, chars, length);
}

// Credit: https://stackoverflow.com/questions/9960908/permutations-in-javascript
function getArrayPermutations(array: any[]) {
  // let copy = array.slice();
  let length = array.length;
  let result = [array.slice()];
  let c = new Array(length).fill(0);
  let i = 1,
    k,
    p;

  while (i < length) {
    if (c[i] < i) {
      k = i % 2 && c[i];
      p = array[i];
      array[i] = array[k];
      array[k] = p;
      ++c[i];
      i = 1;
      result.push(array.slice());
    } else {
      c[i] = 0;
      ++i;
    }
  }
  return result;
}

const getArrayPermutationsMemoize = (array: any[]) => {
  let result: any[] = [];
  const permute = (arr: any[], m: any[] = []) => {
    if (arr.length === 0) {
      result.push(m);
    } else {
      for (let i = 0; i < arr.length; i++) {
        let curr = arr.slice();
        let next = curr.splice(i, 1);
        memoize(permute, curr.slice(), m.concat(next));
      }
    }
  };
  memoize(permute, array);
  return result;
};

function getStringPermutations(str: string) {
  // return [...new Set(getArrayPermutationsMemoize(str.split('')).map(s => s.join('')))];
  return getArrayPermutationsMemoize(str.split('')).map(s => s.join(''));
}

function filterPermutations(permutations: string[], unknown, target) {
  return permutations.filter(p => {
    let guess = '';
    let i = 0;
    let pi = 0;
    for (; i < unknown.length; i++) {
      if (unknown[i] === '?') {
        guess += p[pi++];
      } else {
        guess += unknown[i];
      }
    }

    let verify = [...guess.matchAll(/([\#]+)/g)].map(m => m[0]);
    return verify.map(s => s.length).join(',') === target;
  });
}

function filterPermutations2(permutations: string[], unknown, target) {
  return permutations.filter(p => {
    let unknownCopy = unknown;
    for (let i = 0, j = 0; i < unknown.length; i++) {
      if (unknownCopy[i] === '?') {
        unknownCopy = unknownCopy.slice(0, i) + p[j] + unknownCopy.slice(i + 1);
        j += 1;
      }
    }
    let verify = [...unknownCopy.matchAll(/([\#]+)/g)].map(m => m[0]);
    return verify.map(s => s.length).join(',') === target;
  });
}

// Current is the guessing group strings. Groups are target group numbers
function countArrangementsOld(unknowns: string[], targets: number[]): number {
  const pound = (unknowns: string[], targets: number[], history: string): number => {
    console.log('#', unknowns, targets, history);
    // Ran out of target
    if (unknowns.length !== 0 && targets.length === 0) {
      return 0;
    }
    let nextUnknown = unknowns[0].slice(1);
    let nextUnknowns: string[] = [];
    let nextTargets: number[] = [];
    if (targets[0] === 1 && (nextUnknown[0] === '.' || nextUnknown.length === 0)) {
      // Completed a group
      nextTargets = targets.slice(1);
    } else {
      // Consomed a # so decrease current target group
      let nextTarget = targets[0] - 1;
      // Cannot be negative # count
      if (nextTarget < 0) return 0;
      // Continue target group
      nextTargets = [nextTarget, ...targets.slice(1)];
    }
    // Clean up any empty unknown groups
    if (nextUnknown.length === 0) {
      nextUnknowns = unknowns.slice(1);
    } else {
      nextUnknowns = [nextUnknown, ...unknowns.slice(1)];
    }

    return count(nextUnknowns, nextTargets, history);
  };
  const dot = (unknowns: string[], targets: number[], history: string): number => {
    console.log('.', unknowns, targets, history);
    let nextUnknown = unknowns[0].slice(1);
    // Decrease current unknown string length or finish current guessing group string
    let nextUnknowns =
      nextUnknown.length === 0 ? unknowns.slice(1) : [nextUnknown, ...unknowns.slice(1)];
    // Completed a group
    if (targets[0] === 0) {
      targets = targets.slice(1);
    }
    return count(nextUnknowns, targets, history);
  };
  const count = (unknowns: string[], targets: number[], history: string = ''): number => {
    console.log('count', unknowns, targets, history);
    // Cannot fulfill target
    if (unknowns.length === 0) {
      if (targets.length === 0) {
        console.log('history', unknowns, targets, history);
        return 1;
      } else {
        return 0;
      }
    }
    if (targets.length > 1 && unknowns.join('').length < targets.reduce((a, b) => a + b)) {
      // Cannot fulfill target
      return 0;
    }
    // Last unknown and target to consume
    if (unknowns[0].length === 1 && targets.length === 1 && [0, 1].includes(targets[0])) {
      console.log('history', unknowns, targets, history);
      return 1;
    }
    let arrangements = 0;
    if (unknowns[0][0] === '#') {
      arrangements = pound(unknowns, targets, history + '#');
    } else if (unknowns[0][0] === '.') {
      // Skip over dot
      arrangements = dot(unknowns, targets, history + '.');
    } else if (unknowns[0][0] === '?') {
      arrangements =
        pound(unknowns, targets, history + '#') + dot(unknowns, targets, history + '.');
    }
    return arrangements;
  };
  return count(unknowns, targets);
}

function sum(...nums: number[] | (readonly number[])[]): number {
  let tot = 0;
  for (const x of nums) {
    if (typeof x === 'number') {
      tot += x;
    } else {
      for (const y of x) {
        tot += y;
      }
    }
  }
  return tot;
}

// Credit: https://gist.github.com/Nathan-Fenner/781285b77244f06cf3248a04869e7161
const countWays = memoizeFn((line: string, runs: readonly number[]): number => {
  if (line.length === 0) {
    if (runs.length === 0) {
      return 1;
    }
    return 0;
  }
  if (runs.length === 0) {
    for (let i = 0; i < line.length; i++) {
      if (line[i] === '#') {
        return 0;
      }
    }
    return 1;
  }

  if (line.length < sum(runs) + runs.length - 1) {
    // The line is not long enough for all runs
    return 0;
  }

  if (line[0] === '.') {
    return countWays(line.slice(1), runs);
  }
  if (line[0] === '#') {
    const [run, ...leftoverRuns] = runs;
    for (let i = 0; i < run; i++) {
      if (line[i] === '.') {
        return 0;
      }
    }
    if (line[run] === '#') {
      return 0;
    }

    return countWays(line.slice(run + 1), leftoverRuns);
  }
  // Otherwise dunno first spot, pick
  return countWays('#' + line.slice(1), runs) + countWays('.' + line.slice(1), runs);
});

// Credit: https://www.youtube.com/watch?v=g3Ms5e7Jdqo
function countArrangements(unknowns: string, targets: number[]): number {
  // No more unknowns
  if (unknowns.length === 0) {
    // Check if targets are all consumed
    if (targets.length === 0) return 1;
    else return 0;
  }
  // No more targets
  if (targets.length === 0) {
    // Leftover #
    if (unknowns.includes('#')) return 0;
    else return 1;
  }
  let arrangements = 0;
  if (['.', '?'].includes(unknowns[0])) {
    arrangements += countArrangements(unknowns.slice(1), targets);
  }
  if (['#', '?'].includes(unknowns[0])) {
    if (
      targets[0] <= unknowns.length &&
      !unknowns.slice(0, targets[0]).includes('.') &&
      (targets[0] === unknowns.length || unknowns[targets[0]] !== '#')
    ) {
      // Start a new block
      arrangements += countArrangements(unknowns.slice(targets[0] + 1), targets.slice(1));
    }
  }
  return arrangements;
}

function countArrangementsMemoized(unknowns: string, targets: number[]): number {
  let memoizedFn = memoizeFn((unknowns: string, targets: number[]): number => {
    // No more unknowns
    if (unknowns.length === 0) {
      // Check if targets are all consumed
      if (targets.length === 0) return 1;
      else return 0;
    }
    // No more targets
    if (targets.length === 0) {
      // Leftover #
      if (unknowns.includes('#')) return 0;
      else return 1;
    }
    let arrangements = 0;
    if (['.', '?'].includes(unknowns[0])) {
      arrangements += memoizedFn(unknowns.slice(1), targets);
    }
    if (['#', '?'].includes(unknowns[0])) {
      if (
        targets[0] <= unknowns.length &&
        !unknowns.slice(0, targets[0]).includes('.') &&
        (targets[0] === unknowns.length || unknowns[targets[0]] !== '#')
      ) {
        // Start a new block
        arrangements += memoizedFn(unknowns.slice(targets[0] + 1), targets.slice(1));
      }
    }
    return arrangements;
  });
  return memoizedFn(unknowns, targets);
}

function part1(input: string) {
  let lines = input.trim().split('\n');
  let sum = 0;

  for (let line of lines) {
    let [unknown, target] = line.split(' ');
    // let qCount = unknown.split('?').length - 1;
    // let permutations = getPermutationsMemoize(['#', '.'], qCount);
    // let filtered = filterPermutations(permutations, unknown, target);
    // sum += filtered.length;

    // let unknowns = unknown.split('.').filter(s => s.length !== 0);
    // let targets = target.split(',').map(Number);

    let arrangements = countArrangements(unknown, target.split(',').map(Number));
    sum += arrangements;
  }
  return sum;
}

function part2(input: string) {
  let lines = input.trim().split('\n');
  let sum = 0;

  for (let line of lines) {
    let [unknown, target] = line.split(' ');
    let current = [unknown, unknown, unknown, unknown, unknown].join('?');
    let groups = [target, target, target, target, target].join(',');
    sum += countArrangementsMemoized(current, groups.split(',').map(Number));
  }
  return sum;
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
