const fs = require('fs');
const path = require('path');

function parseTickets(tickets) {
  const ROWS = 128;
  const COLS = 8;
  const seatIds = [];

  for (const ticket of tickets) {
    const chars = ticket.split('');
    let front = 0;
    let back = ROWS - 1;
    let left = 0;
    let right = COLS - 1;
    for (const char of chars) {
      let row = back - front;
      let col = right - left;
      if (char === 'F') {
        back -= Math.ceil(row / 2);
      } else if (char === 'B') {
        front += Math.ceil(row / 2);
      } else if (char === 'L') {
        right -= Math.ceil(col / 2);
      } else if (char === 'R') {
        left += Math.ceil(col / 2);
      }
    }
    const seatId = (front * COLS) + right;
    seatIds.push(seatId);
  }
  return seatIds;
}

function part1(input) {
  const tickets = input
    .trim()
    .split('\n');

  const seatIds = parseTickets(tickets);

  return seatIds.reduce((cur, acc) => {
    if (cur > acc) acc = cur;
    return acc;
  });
}

function part2(input) {
  const tickets = input
    .trim()
    .split('\n');

  const seatIds = parseTickets(tickets);

  const maxId = seatIds.reduce((cur, acc) => {
    if (cur > acc) acc = cur;
    return acc;
  });
  for (let i = 1; i < maxId - 1; i++) {
    if (!seatIds.includes(i) &&
      seatIds.includes(i + 1) &&
      seatIds.includes(i - 1)) {
      return i;
    }
  }
  return -1;
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day5 part1:', part1(input));
console.log('day5 part2:', part2(input));

/*
const input = require('../input');
const log = console.log;
const compose = (...fns) => (args) => fns.reduceRight((arg, fn) => fn(arg), args);
const replace = (...args) => (string) => string.replace(...args);

const replaceUpperHalfsWithOne = replace(/[BR]/g, 1);
const replaceLowerHalfsWithZero = replace(/[FL]/g, 0);
const toIntFromBinaryString = (string) => parseInt(string, 2);
const toSeatID = compose(toIntFromBinaryString, replaceLowerHalfsWithZero, replaceUpperHalfsWithOne);

const ascending = (a, b) => a - b;
const isMissingSeatBehind = (id, _, ids) => !ids.includes(id - 1) && ids.includes(id - 2);

const seatIDs = input(__dirname, '/input.txt').split('\n').map(toSeatID);

const solutionOne = Math.max(...seatIDs);
log(`Solution p1. ${solutionOne}`);

const seatInFrontOfOwn = seatIDs.sort(ascending).find(isMissingSeatBehind);
const solutionTwo = seatInFrontOfOwn - 1;
log(`Solution p2. ${solutionTwo}`);
*/
