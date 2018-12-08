const fs = require('fs');
const path = require('path');

const part1 = (input) => {
  const lines = input
    .trim()
    .split('\n');
  const fabricSize = 1000;
  let count = 0;
  let fabric = [];
  for (let i = 0; i < fabricSize; i++) {
    let row = Array(fabricSize).fill('.');
    fabric.push(row);
  }

  for (const line of lines) {
    let [claim, x, y, width, height] = parseLine(line);
    fillFabric(fabric, claim, x, y, width, height);
  }

  for (const row of fabric) {
    count += row.filter(x => x === 'X').length;
  }

  // printFabric(fabric, 505, 795, 30);
  return count;
}

function parseLine(line) {
  let [claim,, position, claimSize] = line.split(' ');
  claim = claim.substring(1);
  position = position.substring(0, position.length - 1);
  let [x, y] = position.split(',');
  let [width, height] = claimSize.split('x');
  x = parseInt(x, 10);
  y = parseInt(y, 10);
  width = parseInt(width, 10);
  height = parseInt(height, 10);
  return [claim, x, y, width, height];
}

function fillFabric(fabric, claim, x, y, width, height) {
  for (let j = y; j < y + height; j++) {
    for (let i = x; i < x + width; i++) {
      if (fabric[j][i] === '.') {
        fabric[j][i] = claim;
      } else {
        fabric[j][i] = 'X';
      }
    }
  }
}

function printFabric(fabric, x, y, size) {
  for (let j = y; j < y + size; j++) {
    console.log(fabric[j].slice(x, x + size).join(''));
  }
}

function isOverlappingClaim(fabric, claim, x, y, width, height) {
  let isOverlapping = false;
  for (let j = y; j < y + height; j++) {
    for (let i = x; i < x + width; i++) {
      if (fabric[j][i] !== claim) {
        isOverlapping = true;
      }
    }
  }
  return isOverlapping;
}

const part2 = (input) => {
  const lines = input
    .trim()
    .split('\n');
  const fabricSize = 1000;
  let targetClaim = null;
  let fabric = [];
  for (let i = 0; i < fabricSize; i++) {
    let row = Array(fabricSize).fill('.');
    fabric.push(row);
  }

  for (const line of lines) {
    let [claim, x, y, width, height] = parseLine(line);
    fillFabric(fabric, claim, x, y, width, height);
  }

  for (const line of lines) {
    let [claim, x, y, width, height] = parseLine(line);
    if(!isOverlappingClaim(fabric, claim, x, y, width, height)) {
      targetClaim = claim;
    }
  }

  return targetClaim;
}

const data = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day3 part1', part1(data));
console.log('day3 part2', part2(data));
