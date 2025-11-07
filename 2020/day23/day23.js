const fs = require('fs');
const path = require('path');

function part1(input) {
  let cups = input.trim().split('').map(Number);
  let currentIndex = 0;
  const max = Math.max(...cups);
  const min = Math.min(...cups);
  for (let move = 1; move <= 100; move++) {
    const currentCup = cups[currentIndex];
    const picked = [];
    for (let i = 1; i <= 3; i++) {
      picked.push(cups[(currentIndex + i) % cups.length]);
    }
    cups = cups.filter(c => !picked.includes(c));
    let destination = currentCup - 1;
    while (destination < min || picked.includes(destination)) {
      if (destination < min) destination = max;
      else destination--;
    }
    const destIndex = cups.indexOf(destination);
    cups.splice(destIndex + 1, 0, ...picked);
    currentIndex = (cups.indexOf(currentCup) + 1) % cups.length;
  }
  const idx1 = cups.indexOf(1);
  const result = cups
    .slice(idx1 + 1)
    .concat(cups.slice(0, idx1))
    .join('');
  return result;
}

function part2(input) {
  const start = input.trim().split('').map(Number);
  const maxCup = 1000000;
  const moves = 10000000;
  const nextCup = new Array(maxCup + 1);
  for (let i = 0; i < start.length - 1; i++) {
    nextCup[start[i]] = start[i + 1];
  }
  nextCup[start[start.length - 1]] = Math.max(...start) + 1;
  for (let i = Math.max(...start) + 1; i < maxCup; i++) {
    nextCup[i] = i + 1;
  }
  nextCup[maxCup] = start[0];
  let current = start[0];
  for (let move = 0; move < moves; move++) {
    const pick1 = nextCup[current];
    const pick2 = nextCup[pick1];
    const pick3 = nextCup[pick2];
    nextCup[current] = nextCup[pick3];
    let destination = current - 1 || maxCup;
    while (destination === pick1 || destination === pick2 || destination === pick3) {
      destination = destination - 1 || maxCup;
    }
    nextCup[pick3] = nextCup[destination];
    nextCup[destination] = pick1;
    current = nextCup[current];
  }
  const first = nextCup[1];
  const second = nextCup[first];
  return first * second;
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

console.log('part1:', part1(input));
console.log('part2:', part2(input));
