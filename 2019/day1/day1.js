const fs = require('fs');
const path = require('path');

function calculateFuel(mass) {
  return Math.floor(mass / 3) - 2;
}

function calculateTotalFuel(mass) {
  let fuelNeeded = 0;
  let fuel = mass;

  while (true) {
    fuel = calculateFuel(fuel);
    if (fuel <= 0) {
      break;
    }
    fuelNeeded += fuel;
  }

  return fuelNeeded;
}

const part1 = (input) => {
  const masses = input
    .trim()
    .split('\n')
    .map(s => parseInt(s, 10));

  return masses
    .map(mass => calculateFuel(mass))
    .reduce((fuel, totalFuel) => totalFuel += fuel, 0);
}

const part2 = (input) => {
  const masses = input
    .trim()
    .split('\n')
    .map(s => parseInt(s, 10));

  return masses
    .map(mass => calculateTotalFuel(mass))
    .reduce((fuel, totalFuel) => totalFuel += fuel, 0);
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day1 part1:', part1(input));
console.log('day1 part2:', part2(input));
