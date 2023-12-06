function getDistanceTraveled(velocity: number, timeLeft: number) {
  return velocity * timeLeft;
}

function part1(input: string) {
  const lines = input.trim().split('\n');
  let times = [...lines[0].matchAll(/(\d+)/g)].map(m => +m[0]);
  let distances = [...lines[1].matchAll(/(\d+)/g)].map(m => +m[0]);
  let matches = Array(times.length).fill(0) as number[];

  for (let race = 0; race < times.length; race++) {
    for (let holdTime = 1; holdTime < distances[race]; holdTime++) {
      let distanceTraveled = getDistanceTraveled(holdTime, times[race] - holdTime);
      if (distanceTraveled > distances[race]) {
        matches[race] += 1;
      }
      if (distanceTraveled <= 0) break;
    }
  }
  return matches.reduce((a, b) => a * b);
}

function part2(input: string) {
  const lines = input
    .trim()
    .split('\n')
    .map(line => line.replaceAll(/\s+/g, ''));
  let time = [...lines[0].matchAll(/(\d+)/g)].map(m => +m[0])[0];
  let distance = [...lines[1].matchAll(/(\d+)/g)].map(m => +m[0])[0];
  let matches = 0;

  for (let holdTime = 1; holdTime < distance; holdTime++) {
    let distanceTraveled = getDistanceTraveled(holdTime, time - holdTime);
    if (distanceTraveled > distance) {
      matches += 1;
    }
    if (distanceTraveled <= 0) break;
  }
  return matches;
}

let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
Time:      7  15   30
Distance:  9  40  200
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
