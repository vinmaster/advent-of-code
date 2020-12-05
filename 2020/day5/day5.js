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
