function part1(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let sum = 0;
  for (let line of lines) {
    let [id, setsStr] = /^Game (\d+)\: (.*)$/.exec(line).slice(1);
    let sets = setsStr.split(';').map(s => s.trim());
    let max = { red: 12, green: 13, blue: 14 };
    let impossible = false;
    for (let set of sets) {
      let total = { red: 0, green: 0, blue: 0 };
      set.split(', ').forEach(s => {
        let c = /^(\d+) (\w+)$/.exec(s).slice(1);
        total[c[1]] += Number(c[0]);
      });
      if (Object.keys(max).some(color => total[color] > max[color])) {
        impossible = true;
      }
    }
    if (!impossible) {
      sum += Number(id);
    }
  }
  return sum;
}

function part2(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let sum = 0;
  for (let line of lines) {
    let sets = line
      .replace(/Game \d+\: /, '')
      .split(';')
      .map(p => p.trim());
    let max = { red: -1, green: -1, blue: -1 };
    for (let set of sets) {
      set.split(', ').forEach(s => {
        let c = /^(\d+) (\w+)$/.exec(s).slice(1);
        if (max[c[1]] < c[0]) {
          max[c[1]] = Number(c[0]);
        }
      });
    }
    sum += Object.values(max).reduce((product, n) => product * n);
  }
  return sum;
}

let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

// input = `
// Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
// Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
// Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
// Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
// Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
// `;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
