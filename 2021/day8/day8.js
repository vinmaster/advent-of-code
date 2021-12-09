const fs = require('fs');
const path = require('path');

const part1 = input => {
  /** @type string[] */
  let lines = input
    .trim()
    .split('\n');

  let numberSegments = { 2: '1', 4: '4', 3: '7', 7: '8' }

  let result = 0;
  for (let line of lines) {
    let [signalsRaw, outputsRaw] = line.split(' | ');
    let outputs = outputsRaw.split(' ');
    result += outputs.filter(o => numberSegments[o.length] !== undefined).length;
  }
  return result;
};

const part2 = input => {
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
// gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce`

console.log('day8 part1:', part1(input));
console.log('day8 part2:', part2(input));
