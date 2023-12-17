const fs = require('fs');
const path = require('path');

/***
 * Use chars of word2 and take away from chars of word1.
 * Example: 'cf' - 'abdefg' = 'c'
 * @param {string} word1 - Original word
 * @param {string} word2 - Word to use to take away from word1
 * @return {string}
 */
function subtract(word1, word2) {
  return word1
    .split('')
    .filter(c => !word2.includes(c))
    .join('');
}

function objectInvert(obj) {
  return Object.entries(obj).reduce((newObj, entry) => {
    newObj[entry[1]] = entry[0];
    return newObj;
  }, {});
}

/*
7-segment display using a-g for each segment

 aaaa 
b    c
b    c
 dddd 
e    f
e    f
 gggg
*/
const part1 = input => {
  /** @type string[] */
  let lines = input.trim().split('\n');

  let numberSegments = { 2: '1', 4: '4', 3: '7', 7: '8' };

  let result = 0;
  for (let line of lines) {
    let [signalsRaw, outputsRaw] = line.split(' | ');
    let outputs = outputsRaw.split(' ');
    result += outputs.filter(o => numberSegments[o.length] !== undefined).length;
  }
  return result;
};

/*
1. We can decode 1, 4, 7, and 8 digits by looking at the length of the characters. The same goes for the input signal as well.
2. We know that three-three digits have five and six character lengths, respectively.
3. Out of those three digits, if we can find two digits, then the left-over is the 3rd digit.

6 is only one without full overlap with 1
9 is 6 digit number that shares full overlap with 4
0 remainer 6 digit number
3 is the only 5 digit number with full overlap 1
2 has 2 difference vs 4 has 1 difference
*/
const part2 = input => {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let numberSegments = { 2: '1', 4: '4', 3: '7', 7: '8' };
  let sum = 0;
  for (let line of lines) {
    let [signalsRaw, outputsRaw] = line.split(' | ');
    let signals = signalsRaw.split(' ');
    let outputs = outputsRaw.split(' ');
    let lenToWord = signals.reduce((acc, word) => {
      if (numberSegments[word.length]) acc[numberSegments[word.length]] = word;
      return acc;
    }, {});

    for (let signal of signals) {
      if (signal.length === 6 && subtract(lenToWord['1'], signal).length === 1)
        lenToWord['6'] = signal;
    }
    for (let signal of signals) {
      if (
        signal.length === 6 &&
        subtract(lenToWord['4'], signal).length === 1 &&
        !Object.values(lenToWord).includes(signal)
      )
        lenToWord['0'] = signal;
    }
    for (let signal of signals) {
      if (signal.length === 6 && !Object.values(lenToWord).includes(signal))
        lenToWord['9'] = signal;
    }
    for (let signal of signals) {
      if (signal.length === 5 && subtract(lenToWord['6'], signal).length === 1)
        lenToWord['5'] = signal;
    }
    for (let signal of signals) {
      if (
        signal.length === 5 &&
        subtract(lenToWord['9'], signal).length === 1 &&
        !Object.values(lenToWord).includes(signal)
      )
        lenToWord['3'] = signal;
    }
    for (let signal of signals) {
      if (signal.length === 5 && !Object.values(lenToWord).includes(signal))
        lenToWord['2'] = signal;
    }
    // Sort the chars
    for (let key of Object.keys(lenToWord)) {
      lenToWord[key] = lenToWord[key].split('').sort().join('');
    }
    let mapping = objectInvert(lenToWord);
    let num = Number(outputs.map(o => mapping[o.split('').sort().join('')]).join(''));
    sum += num;
  }
  return sum;
};

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

// input = `
// be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
// edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
// fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
// fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
// aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
// fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
// dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
// bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
// egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
// gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce`;

console.log('day8 part1:', part1(input));
console.log('day8 part2:', part2(input));
