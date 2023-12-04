function part1(input: string) {
  const lines = input.trim().split('\n');
  let points = 0;
  for (let line of lines) {
    let [winningStr, mineStr] = line.replace(/.*: /, '').split('|');
    let winning = [...winningStr.matchAll(/(\d+)/g)].map(m => +m[0]);
    let mine = [...mineStr.matchAll(/(\d+)/g)].map(m => +m[0]);
    let count = mine.filter(n => winning.includes(n)).length;
    if (count !== 0) {
      points += Math.pow(2, count - 1);
    }
  }
  return points;
}

function part2(input: string) {
  const lines = input.trim().split('\n');
  let cards = Array(lines.length).fill(1);
  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];
    let [winningStr, mineStr] = line.replace(/.*: /, '').split('|');
    let winning = [...winningStr.matchAll(/(\d+)/g)].map(m => +m[0]);
    let mine = [...mineStr.matchAll(/(\d+)/g)].map(m => +m[0]);
    let count = mine.filter(n => winning.includes(n)).length;
    // Repeat for how many current copies of card
    for (let times = 0; times < cards[i]; times++) {
      // Add next cards
      for (let j = 0; j < count; j++) {
        if (i + 1 < lines.length) {
          cards[i + j + 1] += 1;
        }
      }
    }
  }
  return cards.reduce((a, b) => a + b);
}

let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

// input = `
// Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
// Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
// Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
// Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
// Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
// Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
// `;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
