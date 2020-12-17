const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
// util.inspect.defaultOptions.showHidden = true;
// util.inspect.defaultOptions.depth = null;
// util.inspect.defaultOptions.compact = true;

const log = (...args) => console.log(...args);
const isBetween = (low, high, val) => val >= low && val <= high;
const sum = (acc, cur) => acc + cur;

function isValidField(rulesList, field) {
  return rulesList.some(rule => {
    let [min, max] = rule.split('-').map(Number);
    return isBetween(min, max, Number(field));
  });
}

function isValidField2(rulesMap, field) {
  return Object.entries(rulesMap).some(([_, rules]) => {
    return isValidField(rules, field);
  })
}

function part1(input) {
  let [rules, yourTicket, nearByTickets] = input
    .trim()
    .split('\n\n');

  // Clean input
  rules = rules.split('\n');
  yourTicket = yourTicket.split('\n')[1];
  nearByTickets = nearByTickets.split('\n');
  nearByTickets.shift();

  // Parse rules
  let rulesList = [];
  for (let rule of rules) {
    let [_, range1, range2] = rule.match(/^.* (\d+-\d+) or (\d+-\d+)$/);
    rulesList.push(range1, range2);
  }
  let invalidFields = [];
  for (let ticket of nearByTickets) {
    for (let field of ticket.split(',')) {
      if (!isValidField(rulesList, field)) {
        invalidFields.push(field);
      }
    }
  }

  return invalidFields.map(Number).reduce(sum, 0);
}

function part2(input) {
  let [rules, yourTicket, nearByTickets] = input
    .trim()
    .split('\n\n');

  // Clean input
  rules = rules.split('\n');
  yourTicket = yourTicket.split('\n')[1].split(',');
  nearByTickets = nearByTickets.split('\n');
  nearByTickets.shift();

  // Parse rules
  let rulesMap = {};
  for (let rule of rules) {
    let [_, field, range1, range2] = rule.match(/^(.*): (\d+-\d+) or (\d+-\d+)$/);
    rulesMap[field] = [range1, range2];
  }

  // Only take valid tickets
  let validTickets = [];
  for (let ticket of nearByTickets) {
    let isValid = ticket.split(',').every(field => isValidField2(rulesMap, field))
    if (isValid) validTickets.push(ticket);
  }

  // Calculate which field can be possible for each value
  let possibleAtPosition = [];
  yourTicket.map(field => possibleAtPosition.push(Object.keys(rulesMap)));
  for (let ticket of validTickets) {
    let values = ticket.split(',').map(Number);
    for (let position = 0; position < values.length; position++) {
      let possibleFields = possibleAtPosition[position].filter(field => {
        return isValidField(rulesMap[field], values[position]);
      });
      possibleAtPosition[position] = possibleFields;
    }
  }

  // Narrow down by removing found field from other possible field
  while (true) {
    for (let i = 0; i < possibleAtPosition.length; i++) {
      if (possibleAtPosition[i].length === 1) {
        for (let j = 0; j < possibleAtPosition.length; j++) {
          if (i !== j) {
            possibleAtPosition[j] = possibleAtPosition[j].filter(value => value !== possibleAtPosition[i][0]);
          }
        }
      }
    }
    if (possibleAtPosition.every(list => list.length === 1)) {
      break;
    }
  }

  // Get the numbers 
  let solution = [];
  for (let i = 0; i < possibleAtPosition.length; i++) {
    if (possibleAtPosition[i][0].includes('departure')) {
      solution.push(yourTicket[i]);
    }
  }

  return solution.map(Number).reduce((acc, cur) => acc * cur);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day16 part1:', part1(input));
log('day16 part2:', part2(input));
