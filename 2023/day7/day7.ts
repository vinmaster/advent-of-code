interface Hand {
  cards: string;
  bid: number;
}

function frequencies(arr: string[]) {
  return arr.reduce(function (value, value2) {
    return value[value2] ? ++value[value2] : (value[value2] = 1), value;
  }, {});
}

function getType(cards: string): number {
  let freq = frequencies(cards.split(''));
  if (Object.values(freq).includes(5)) {
    // 5 of a kind
    return 7;
  } else if (Object.values(freq).includes(4)) {
    // 4 of a kind
    return 6;
  } else if (Object.values(freq).includes(3) && Object.keys(freq).length === 2) {
    // Full house
    return 5;
  } else if (Object.values(freq).includes(3) && Object.keys(freq).length === 3) {
    // 3 of a kind
    return 4;
  } else if (Object.values(freq).includes(2) && Object.keys(freq).length === 3) {
    // 2 pair
    return 3;
  } else if (Object.values(freq).includes(2) && Object.keys(freq).length === 4) {
    // 1 pair
    return 2;
  } else {
    // High card
    return 1;
  }
}

function byRank(hand1: Hand, hand2: Hand): number {
  let typeDiff = getType(hand1.cards) - getType(hand2.cards);
  if (typeDiff !== 0) return typeDiff;

  let strength = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2'];
  for (let i = 0; i < hand1.cards.length; i++) {
    let strengthDiff = strength.indexOf(hand2.cards[i]) - strength.indexOf(hand1.cards[i]);
    if (strengthDiff !== 0) return strengthDiff;
  }

  return typeDiff;
}

function getBestHand(cards: string) {
  let freq = frequencies(cards.split(''));
  if (freq['J'] === undefined) {
    return cards;
  } else {
    let j = freq['J'];
    delete freq['J'];
    let entries = Object.entries(freq) as [string, number][];
    if (entries.length === 0) {
      return 'AAAAA';
    }
    entries.sort((a, b) => b[1] - a[1]);
    entries[0][1] += j;
    let newHand = entries.map(e => e[0].repeat(e[1])).join('');
    return newHand;
  }
}

function byRankWildJ(hand1: Hand, hand2: Hand): number {
  let typeDiff = getType(getBestHand(hand1.cards)) - getType(getBestHand(hand2.cards));
  if (typeDiff !== 0) return typeDiff;

  let strength = ['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J'];
  for (let i = 0; i < hand1.cards.length; i++) {
    let strengthDiff = strength.indexOf(hand2.cards[i]) - strength.indexOf(hand1.cards[i]);
    if (strengthDiff !== 0) return strengthDiff;
  }

  return typeDiff;
}

function part1(input: string) {
  const lines = input.trim().split('\n');
  let rounds = [] as Hand[];
  for (let line of lines) {
    let [cards, bidStr] = line.split(' ');
    let bid = Number(bidStr);
    rounds.push({ cards, bid });
  }
  rounds.sort(byRank);
  return rounds.map((hand, rank) => hand.bid * (rank + 1)).reduce((a, b) => a + b);
}

function part2(input: string) {
  const lines = input.trim().split('\n');
  let rounds = [] as Hand[];
  for (let line of lines) {
    let [cards, bidStr] = line.split(' ');
    let bid = Number(bidStr);
    rounds.push({ cards, bid });
  }
  rounds.sort(byRankWildJ);
  return rounds.map((hand, rank) => hand.bid * (rank + 1)).reduce((a, b) => a + b);
}

let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
