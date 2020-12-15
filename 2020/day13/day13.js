const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
// util.inspect.defaultOptions.showHidden = true;
// util.inspect.defaultOptions.depth = null;
// util.inspect.defaultOptions.compact = true;

const log = (...args) => console.log(...args);
const modulo = (x, m) => {
  while (x < 0) x += m;
  return x % m;
};

// Chinese Remainder Theorem
// https://github.com/pnicorelli/nodejs-chinese-remainder
function mul_inv(a, b) {
  var b0 = b;
  var x0 = BigInt(0);
  var x1 = BigInt(1);
  var q, tmp;
  if (b == BigInt(1)) {
    return BigInt(1);
  }
  while (a > 1) {
    q = (a / b);
    tmp = a;
    a = b;
    b = tmp % b;
    tmp = x0;
    x0 = x1 - (q * x0);
    x1 = tmp;
  }
  if (x1 < BigInt(0)) {
    x1 = x1 + b0;
  }
  return x1;
}

function chineseRemainder(a, n) {
  var p = BigInt(1);
  var i = BigInt(1);
  var prod = BigInt(1);
  var sm = BigInt(0);
  for (i = 0; i < n.length; i++) {
    prod = prod * n[i];
  }
  for (i = 0; i < n.length; i++) {
    p = prod / n[i];
    sm = sm + (a[i] * mul_inv(p, n[i]) * p);
  }
  return sm % prod;
}

function part1(input) {
  let lines = input
    .trim()
    .split('\n');
  let time = parseInt(lines[0], 10);
  let busIds = lines[1].split(',').map(Number).filter(t => !isNaN(t));

  let timeAfters = [];
  for (let busId of busIds) {
    let timeBefore = time % busId;
    let timeAfter = (time - timeBefore) + busId;
    timeAfters.push(timeAfter);
  }
  let min = Math.min(...timeAfters.map(t => t - time));

  let minIndex = timeAfters.indexOf(min + time);
  let waitTime = timeAfters[minIndex] - time;

  return busIds[minIndex] * waitTime;
}

function part2(input) {
  let lines = input
    .trim()
    .split('\n');
  let busIds = lines[1].split(',').map(Number);

  let a = [];
  for (let i = 0; i < busIds.length; i++) {
    if (isNaN(busIds[i])) continue;
    a.push(BigInt(modulo(-i, busIds[i])));
  }

  let n = busIds.filter(id => !isNaN(id)).map(BigInt);
  return Number(chineseRemainder(a, n));
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day13 part1:', part1(input));
log('day13 part2:', part2(input));

/*
const input = require('../input');
const log = console.log;
const not = (fn) => (...args) => !fn(...args);
const isNumber = not(isNaN);
const first = ([first]) => first;

const withNextDeparture = (timestamp) => (id) => ({ id, departure: Math.ceil(timestamp / id) * id });

const findEarliestDepartingBus = (timestamp, busses) =>
  first(
    busses
      .filter(isNumber)
      .map(withNextDeparture(timestamp))
      .sort((a, b) => a.departure - b.departure),
  );

const findEarliestTimestampOfSubsequentDepartures = (busses) =>
  busses.reduce(
    ({ timestamp, period }, bus, index) => {
      if (isNaN(bus)) return { timestamp, period };

      while ((timestamp + index) % bus) {
        timestamp += period;
      }

      return { timestamp, period: period * bus };
    },
    { timestamp: 0, period: 1 },
  );

let [timestamp, busses] = input(__dirname, './input.txt').split('\n');
busses = busses.split(',');

const bus = findEarliestDepartingBus(timestamp, busses);
log(`Solution pt.1 ${bus.id * (bus.departure - timestamp)}`);

const earliest = findEarliestTimestampOfSubsequentDepartures(busses);
log(`Solution pt.2 ${earliest.timestamp}`);
*/
