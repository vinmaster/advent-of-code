const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
// util.inspect.defaultOptions.showHidden = true;
// util.inspect.defaultOptions.depth = null;
// util.inspect.defaultOptions.compact = true;

const insertAt = (str, value, pos) => `${str.slice(0, pos)}${value}${str.slice(pos)}`;

/** Addition and multiplication have same precedence
 * @param {string} str
 */
function resolveExpression(str) {
  let lastOpenParans = [0];
  for (let i = 0; i < str.length; i++) {
    if (str[i] === '(') {
      lastOpenParans.unshift(i);
    } else if (str[i] === ')') {
      lastOpenParans.shift();
    } else if (['+', '*'].includes(str[i])) {
      str = insertAt(str, ')', i);
      let openParan = lastOpenParans[0];
      str = insertAt(str, '(', openParan);
      i += 2;
    }
  }
  return str;
}

/** Addition is evaluated before multiplication
 * @param {string} str
 */
function resolveExpression2(str) {
  let lastOpenParans = [0];
  for (let i = 0; i < str.length; i++) {
    if (str[i] === '(') {
      lastOpenParans.unshift(i);
    } else if (str[i] === ')') {
      lastOpenParans.shift();
    } else if (str[i] === '+') {
      // Find opening paren backwards or insert at previous number
      if (str[i - 1] === ')') {
        let parenLevel = 1;
        for (let j = i - 1; j >= 0; j--) {
          if (str[j] === ')') parenLevel += 1;
          else if (str[j] === '(') {
            parenLevel -= 1;
            if (parenLevel === 1) {
              str = insertAt(str, '(', j);
              break;
            }
          }
        }
      } else {
        str = insertAt(str, '(', i - 1);
      }

      // Find closing paren forward or insert at next number
      if (str[i + 2] === '(') {
        let parenLevel = 1;
        for (let j = i + 1; j < str.length; j++) {
          if (str[j] === '(') parenLevel += 1;
          else if (str[j] === ')') {
            parenLevel -= 1;
            if (parenLevel === 1) {
              str = insertAt(str, ')', j);
              break;
            }
          }
        }
      } else {
        str = insertAt(str, ')', i + 3);
      }
      i += 2;
    }
  }
  return str;
}

function part1(input) {
  let lines = input
    .trim()
    .split('\n');

  lines = lines.map(l => l.replace(/\s/g, ''));

  return lines
    .map(l => eval(resolveExpression(l)))
    .reduce((sum, num) => sum + num);
}

function part2(input) {
  let lines = input
    .trim()
    .split('\n');

  lines = lines.map(l => l.replace(/\s/g, ''));

  return lines
    .map(l => eval(resolveExpression2(l)))
    .reduce((sum, num) => sum + num);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

console.log('day18 part1:', part1(input));
console.log('day18 part2:', part2(input));

// Matches inner most paren exp only
// let test = '7*(7*6+7*5+(9+2*2*4))+7*3*5';
// console.log(/\([^()]+\)/.exec(test));

/*
https://en.wikipedia.org/wiki/Operator-precedence_parser#Alternative_methods
*/
// console.log(require('fs').readFileSync('input', 'utf8').trim().split('\n')
//     .map(l => '((' + l
//         .replace(/\(/g, '(((')
//         .replace(/\)/g, ')))')
//         .replace(/\+/g, ')+(')
//         .replace(/\*/g, '))*((')
//         + '))')
//     .map(l => eval(l))
//     .reduce((p, c) => p + c, 0));

/*
const input = require('./data');

const PARENS_RE = /\(([^()]+)\)/;
const ORDERED_RE = /(\d+) (\*|\+) (\d+)/;
const ADD_RE = /(\d+) \+ (\d+)/;
const MULT_RE = /(\d+) \* (\d+)/;

const transformer = (re, replacer) => str => {
  let match = null;
  while ((match = str.match(re)) !== null) {
    str = str.replace(match[0], replacer(match))
  }
  return str;
};

const pipe = (str, ...args) => args.reduce((acc, curr) => curr(acc), str)

const evaluate = (expr, advanced = false) => parseInt(
  advanced
    ? pipe(
      expr,
      transformer(PARENS_RE, ([_, str]) => evaluate(str, advanced)),
      transformer(ADD_RE, ([_, a, b]) => parseInt(a) + parseInt(b)),
      transformer(MULT_RE, ([_, a, b]) => parseInt(a) * parseInt(b))
    ) : pipe(
      expr,
      transformer(PARENS_RE, ([_, str]) => evaluate(str, advanced)),
      transformer(ORDERED_RE, ([match]) => eval(match))
    )
)

console.log(
  input.reduce((acc, curr) => ({
    part1: (acc.part1 || 0) + evaluate(curr),
    part2: (acc.part2 || 0) + evaluate(curr, true)
  }), {})
);

--------------------------------------------------------------------------------

const fs = require('fs');
const expressions = fs.readFileSync('./input.txt', 'utf-8').split('\n');

function calc (expression) {
  // if it's just a number, do nothing
  if (!isNaN(Number(expression))) { Number(expression) }
  // if there's brackets, do that first
  while (expression.match(/\(/)){
    expression = expression.replace(/\([^()]+\)/, match =>
      calc (match.slice(1, match.length-1))
    )
  }
  // do all the calculations until it's just a number
  while (isNaN(Number(expression))) {
    expression = expression.replace(/(\d+) ([+*]) (\d+)/, (match, a, op, b) =>
      op == '+' ? parseInt(a) + parseInt(b) : parseInt(a) * parseInt(b)
    )
  }
  return Number(expression)
}

let runningTotal = 0
for (expression of expressions) {
  runningTotal += calc (expression)
}
console.log(runningTotal)



--------------------------------------------------------------------------------
*/

// const fs = require('fs');
// const expressions = fs.readFileSync('./input.txt', 'utf-8').split('\n');

// function calc (expression) {
//   // if it's just a number, do nothing
//   if (!isNaN(Number(expression))) { Number(expression) }
//   // if there's brackets, do that first
//   while (expression.match(/\(/)){
//     expression = expression.replace(/\([^()]+\)/, match => 
//       calc (match.slice(1, match.length-1))
//     )
//   }
//   // if there's addition, do that
//   while (expression.match(/\+/)){
//     expression = expression.replace(/(\d+) \+ (\d+)/, (match, a, b) => 
//       parseInt(a) + parseInt(b)
//     )
//   }
//   // if there's multiplication, do that
//   while (expression.match(/\*/)) {
//     expression = expression.replace(/(\d+) \* (\d+)/, (match, a, b) => 
//       parseInt(a) * parseInt(b)
//     )
//   }
//   return Number(expression)
// }

// let runningTotal = 0
// for (expression of expressions) {
//   runningTotal += calc (expression)
// }
// console.log(runningTotal)
