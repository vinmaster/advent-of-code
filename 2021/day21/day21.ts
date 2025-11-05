type Input = number[];

function* numGenerator() {
  let num = 1;
  while (true) {
    yield num;
    num += 1;
  }
}

export function part1(input: Input) {
  let playerPositions = input;
  let playerScores = [0, 0];
  let currentPlayer = 0;
  let spaces = 10;
  let die = numGenerator();
  let rolls = 0;
  let winScore = 1000;

  while (Math.max(...playerScores) < winScore) {
    let move = die.next().value! + die.next().value! + die.next().value!;
    rolls += 3;
    playerPositions[currentPlayer] = (playerPositions[currentPlayer] + move) % spaces;
    playerScores[currentPlayer] +=
      playerPositions[currentPlayer] === 0 ? 10 : playerPositions[currentPlayer];

    currentPlayer = (currentPlayer + 1) % 2;
  }
  return Math.min(...playerScores) * rolls;
}

export function part2(input: Input) {
  let [p1Start, p2Start] = input;
  let playerScores = [0, 0];
  let currentPlayer = 0;
  let spaces = 10;
  let winScore = 21;
  const memo = new Map<string, [number, number]>();
  // This map stores the 7 possible outcomes of a turn (3 rolls)
  // and the number of universes each outcome happens in.
  const turnOutcomes = new Map<number, number>([
    [3, 1],
    [4, 3],
    [5, 6],
    [6, 7],
    [7, 6],
    [8, 3],
    [9, 1],
  ]);

  /**
   * Recursively counts the number of wins for each player from a given game state.
   * @returns A tuple [p1Wins, p2Wins]
   */
  function countWins(
    p1pos: number,
    p1score: number,
    p2pos: number,
    p2score: number,
    isP1Turn: boolean
  ): [number, number] {
    // Base Case: If a player has already won, the game ends.
    // This is checked at the *start* of the turn.
    if (p1score >= winScore) {
      return [1, 0]; // P1 won in this universe
    }
    if (p2score >= winScore) {
      return [0, 1]; // P2 won in this universe
    }

    // Memoization: Check if we've seen this exact state before.
    const key = `${p1pos},${p1score},${p2pos},${p2score},${isP1Turn}`;
    if (memo.has(key)) {
      return memo.get(key)!;
    }

    // Initialize total wins from this state
    let totalP1Wins = 0;
    let totalP2Wins = 0;

    // Iterate through all 7 possible move sums for the current player
    for (const [move, freq] of turnOutcomes.entries()) {
      if (isP1Turn) {
        // Calculate Player 1's new position and score
        // (pos - 1 + move) % SPACES + 1 is a safe way to wrap 1-based indexing
        const newP1Pos = ((p1pos - 1 + move) % spaces) + 1;
        const newP1Score = p1score + newP1Pos;

        // Recurse for Player 2's turn
        const [p1Wins, p2Wins] = countWins(
          newP1Pos,
          newP1Score,
          p2pos,
          p2score,
          false // It's now P2's turn
        );

        // Add the wins from all sub-universes, multiplied by how many
        // universes this specific move (e.g., "7") created.
        totalP1Wins += p1Wins * freq;
        totalP2Wins += p2Wins * freq;
      } else {
        // It's Player 2's turn

        // Calculate Player 2's new position and score
        const newP2Pos = ((p2pos - 1 + move) % spaces) + 1;
        const newP2Score = p2score + newP2Pos;

        // Recurse for Player 1's turn
        const [p1Wins, p2Wins] = countWins(
          p1pos,
          p1score,
          newP2Pos,
          newP2Score,
          true // It's now P1's turn
        );

        // Add the wins from the subsequent recursive calls
        totalP1Wins += p1Wins * freq;
        totalP2Wins += p2Wins * freq;
      }
    }

    // Cache the result for this state before returning
    memo.set(key, [totalP1Wins, totalP2Wins]);
    return [totalP1Wins, totalP2Wins];
  }

  // Start the game from the initial positions and 0 scores, with P1 going first.
  const [p1Wins, p2Wins] = countWins(p1Start, 0, p2Start, 0, true);

  return Math.max(p1Wins, p2Wins);
}

function parseInput(inputString: string): Input {
  let lines = inputString.trim().split('\n');
  // let matches = lines.map(line => [...line.match(/Player 1 starting position: (\d+)/)?.slice(1)!]);
  let matches = lines.map(line => line.match(/Player . starting position: (\d+)/)!.slice(1));

  return matches.map(m => Number(m[0]));
}

async function main(useRealInput = true) {
  let inputString = '';
  try {
    inputString =
      // @ts-expect-error: next-line
      typeof Bun !== 'undefined'
        ? // @ts-expect-error: next-line
          await Bun.file(`${import.meta.dir}/input.txt`).text()
        : // @ts-expect-error: next-line
        typeof Deno !== 'undefined'
        ? // @ts-expect-error: next-line
          await Deno.readTextFile(`${import.meta.dirname}/input.txt`)
        : '';
  } catch (error) {
    useRealInput = false;
  }

  let testInput = `
Player 1 starting position: 4
Player 2 starting position: 8
`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
