const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
// util.inspect.defaultOptions.showHidden = true;
// util.inspect.defaultOptions.depth = null;
// util.inspect.defaultOptions.compact = true;

/**
 * @param {Map<string, string>} rules 
 * @param {number} ruleNum 
 * @param {string} str 
 */
function getRegExStr(rules, ruleNum) {
  // let hasDigit = new RegExp('\d', 'g');
  let hasDigit = /\d/g;
  let currentRule = rules.get(ruleNum);

  // Check if rules reference other rules
  if (hasDigit.test(currentRule)) {
    // Check for OR conditions
    if (currentRule.includes('|')) {
      let [p1, p2] = currentRule.split(' | ');

      let s1 = '';
      for (let part of p1.split(' ')) {
        s1 += getRegExStr(rules, part);
      }

      let s2 = '';
      for (let part of p2.split(' ')) {
        s2 += getRegExStr(rules, part);
      }

      return `(${s1}|${s2})`;
    } else {
      let s1 = '';
      for (let part of currentRule.split(' ')) {
        s1 += getRegExStr(rules, part);
      }
      return s1;
    }
  } else {
    return currentRule.replace(new RegExp('"', 'g'), '');
  }
}

function consume(rules, rule, input) {
  let match;
  if (match = /^"(\w)"$/.exec(rule)) {
    if (input[0] === match[1]) {
      return [input.slice(1)];
    } else {
      return [];
    }
  } else if (/^(\d+)$/.test(rule)) {
    return consume(rules, rules.get(rule), input);
  } else if (/\|/.test(rule)) {
    const subrules = rule.split(' | ');
    return subrules.map(subrule => consume(rules, subrule, input)).flat();
  } else {
    const subrules = rule.split(' ');
    let result = [input];
    for (let subrule of subrules) {
      result = result.map(x => consume(rules, subrule, x)).flat();
    }
    return result;
  }
}

function part1(input) {
  /** @type {[string string]} */
  let [rulesStr, messagesStr] = input
    .trim()
    .split('\n\n');

  /** @type Map<string, string> */
  let rules = new Map();
  for (let line of rulesStr.split('\n')) {
    let [_, ruleNum, rule] = line.match(/^(\d+)\: (.+)$/);
    rules.set(ruleNum, rule);
  }

  let messages = messagesStr.split('\n');
  let regExStr = `^${getRegExStr(rules, '0')}$`;

  return messages.filter(m => new RegExp(regExStr).test(m)).length;
}

function part2(input) {
  let [rulesStr, messagesStr] = input
    .trim()
    .split('\n\n');

  let rules = new Map();
  for (let line of rulesStr.split('\n')) {
    let [_, ruleNum, rule] = line.match(/^(\d+)\: (.+)$/);
    rules.set(ruleNum, rule);
  }

  // Update rules
  rules.set('8', '42 | 42 8');
  rules.set('11', '42 31 | 42 11 31');

  let messages = messagesStr.split('\n');
  return messages.filter(i => consume(rules, '0', i).includes('')).length;
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

console.log('day19 part1:', part1(input));
console.log('day19 part2:', part2(input));

/*
function run([rules, input]) {
  rules = new Map(rules.map(r => r.split(': ')));
  const re = new RegExp('^'+expand(rules, '0')+'$');
  return input.filter(i => re.test(i)).length;
}

function expand(rules, rule) {
  let match;
  if (match = /^"(\w+)"$/.exec(rule)) {
    return match[1];
  } else if (/^\d+$/.test(rule)) {
    return expand(rules, rules.get(rule));
  } else if (/\|/.test(rule)) {
    return '(' + rule.split(' | ').map(r => expand(rules, r)).join('|') + ')';
  } else {
    return rule.split(' ').map(r => expand(rules, r)).join('');
  }
}

function parseInput(str) {
  return autoparse(str);
}
*/
