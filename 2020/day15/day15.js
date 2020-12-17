const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
// util.inspect.defaultOptions.showHidden = true;
// util.inspect.defaultOptions.depth = null;
// util.inspect.defaultOptions.compact = true;

const log = (...args) => console.log(...args);

function part1(input) {
  let lines = input
    .trim()
    .split(',')
    .map(Number);

  let track = {};
  for (let turn = 0; turn < lines.length; turn++) {
    let current = lines[turn];
    if (!track[current.toString()]) track[current.toString()] = [];
    track[current.toString()].unshift(turn);
  }
  lines.push(0);
  track['0'].unshift(lines.length - 1);

  let target = 2020;
  for (let turn = lines.length; turn < target; turn++) {
    let last = lines[turn - 1];
    if (!track[last.toString()]) track[last.toString()] = [];
    if (track[last.toString()].length < 2) {
      lines.push(0);
      track['0'].unshift(lines.length - 1);
      track['0'] = track['0'].slice(0, 2);
      continue;
    }

    let [index1, index2, _] = track[last.toString()];
    let next = (index1 + 1) - (index2 + 1);
    lines.push(next);
    if (!track[next.toString()]) track[next.toString()] = [];
    track[next.toString()].unshift(turn);
    track[next.toString()] = track[next.toString()].slice(0, 2);
  }
  return lines.pop();
}

function part2(input) {
  let lines = input
    .trim()
    .split(',')
    .map(Number);

  let track = {};
  for (let turn = 0; turn < lines.length; turn++) {
    let current = lines[turn];
    if (!track[current.toString()]) track[current.toString()] = [];
    track[current.toString()].unshift(turn);
  }
  lines.push(0);
  track['0'].unshift(lines.length - 1);

  let target = 30000000;
  for (let turn = lines.length; turn < target; turn++) {
    let last = lines[turn - 1];
    if (!track[last.toString()]) track[last.toString()] = [];
    if (track[last.toString()].length < 2) {
      lines.push(0);
      track['0'].unshift(lines.length - 1);
      track['0'] = track['0'].slice(0, 2);
      continue;
    }

    let [index1, index2, _] = track[last.toString()];
    let next = (index1 + 1) - (index2 + 1);
    lines.push(next);
    if (!track[next.toString()]) track[next.toString()] = [];
    track[next.toString()].unshift(turn);
    track[next.toString()] = track[next.toString()].slice(0, 2);
  }
  return lines.pop();
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day15 part1:', part1(input));
log('day15 part2:', part2(input));

/*
const log = console.log;

const play = (starting, maxTurns, memory = new Map()) => {
  let next;

  for (let turn = 1; turn < maxTurns; turn++) {
    const current = turn <= starting.length ? starting[turn - 1] : next;
    next = memory.has(current) ? turn - memory.get(current) : 0;
    memory = memory.set(current, turn);
  }

  return next;
};

const input = [15, 5, 1, 4, 7, 0];
log(`Solution pt.1 ${play(input, 2020)}`);
log(`Solution pt.2 ${play(input, 30000000)}`);
*/
