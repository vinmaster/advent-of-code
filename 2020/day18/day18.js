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
