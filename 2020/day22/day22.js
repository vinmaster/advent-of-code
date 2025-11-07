const fs = require('fs');
const path = require('path');

// util.inspect.defaultOptions.maxArrayLength = null;
// util.inspect.defaultOptions.showHidden = true;
// util.inspect.defaultOptions.depth = null;
// util.inspect.defaultOptions.compact = true;

// Sort descending
let sort = (a, b) => b - a;

function part1(input) {
  /** @type {[string, string]} */
  let [player1, player2] = input.trim().split('\n\n');

  let deck1 = player1.split('\n').slice(1).map(Number);
  let deck2 = player2.split('\n').slice(1).map(Number);

  while (deck1.length !== 0 && deck2.length !== 0) {
    // for (let i = 0; i < 10; i++) {
    let [card1, card2] = [deck1.shift(), deck2.shift()];

    if (card1 > card2) {
      deck1 = deck1.concat([card1, card2].sort(sort));
    } else {
      deck2 = deck2.concat([card1, card2].sort(sort));
    }
  }

  // console.log(deck1);
  // console.log(deck2);
  let result = deck1.length === 0 ? deck2 : deck1;
  return result
    .reverse()
    .map((val, i) => val * (i + 1))
    .reduce((a, b) => a + b);
}

function part2(input) {
  let [player1, player2] = input.trim().split('\n\n');
  let deck1 = player1.split('\n').slice(1).map(Number);
  let deck2 = player2.split('\n').slice(1).map(Number);
  function playGame(deck1, deck2) {
    const seen = new Set();
    while (deck1.length && deck2.length) {
      const state = deck1.join(',') + '|' + deck2.join(',');
      if (seen.has(state)) return [1, deck1]; // Player 1 wins due to loop prevention
      seen.add(state);
      const card1 = deck1.shift();
      const card2 = deck2.shift();
      let winner;
      // If both players have enough cards for recursion
      if (deck1.length >= card1 && deck2.length >= card2) {
        const [subWinner] = playGame(deck1.slice(0, card1), deck2.slice(0, card2));
        winner = subWinner;
      } else {
        winner = card1 > card2 ? 1 : 2;
      }
      if (winner === 1) {
        deck1.push(card1, card2);
      } else {
        deck2.push(card2, card1);
      }
    }
    return deck1.length ? [1, deck1] : [2, deck2];
  }
  const [winner, winningDeck] = playGame(deck1, deck2);
  return winningDeck
    .slice() // copy
    .reverse()
    .map((val, i) => val * (i + 1))
    .reduce((a, b) => a + b, 0);
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

// input = `
// Player 1:
// 9
// 2
// 6
// 3
// 1

// Player 2:
// 5
// 8
// 4
// 7
// 10
// `

console.log('part1:', part1(input));
console.log('part2:', part2(input));
