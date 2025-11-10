const fs = require('fs');
const path = require('path');

// ---- Direction vectors in axial coordinates ----
const directions = {
  e: [1, 0],
  w: [-1, 0],
  ne: [0, -1],
  nw: [-1, -1],
  se: [1, 1],
  sw: [0, 1],
};

function parsePath(line) {
  const steps = [];
  for (let i = 0; i < line.length; ) {
    if (line[i] === 'n' || line[i] === 's') {
      steps.push(line.slice(i, i + 2));
      i += 2;
    } else {
      steps.push(line[i]);
      i++;
    }
  }
  return steps;
}

function getTileCoords(steps) {
  let q = 0,
    r = 0;
  for (const step of steps) {
    const [dq, dr] = directions[step];
    q += dq;
    r += dr;
  }
  return [q, r];
}

function getNeighbors(q, r) {
  return Object.values(directions).map(([dq, dr]) => [q + dq, r + dr]);
}

function part1(input) {
  const lines = input.trim().split('\n');
  const blackTiles = new Set();

  for (const line of lines) {
    const [q, r] = getTileCoords(parsePath(line));
    const key = `${q},${r}`;
    if (blackTiles.has(key)) blackTiles.delete(key);
    else blackTiles.add(key);
  }

  return blackTiles.size;
}

function part2(input) {
  const lines = input.trim().split('\n');
  let blackTiles = new Set();

  for (const line of lines) {
    const [q, r] = getTileCoords(parsePath(line));
    const key = `${q},${r}`;
    if (blackTiles.has(key)) blackTiles.delete(key);
    else blackTiles.add(key);
  }

  for (let day = 1; day <= 100; day++) {
    const neighborCounts = new Map();
    // Count neighbors for all black tiles
    for (const tile of blackTiles) {
      const [q, r] = tile.split(',').map(Number);
      for (const [nq, nr] of getNeighbors(q, r)) {
        const key = `${nq},${nr}`;
        neighborCounts.set(key, (neighborCounts.get(key) ?? 0) + 1);
      }
    }
    const nextBlackTiles = new Set();
    // Apply flipping rules
    for (const [tile, count] of neighborCounts) {
      const isBlack = blackTiles.has(tile);
      if (isBlack && (count === 1 || count === 2)) nextBlackTiles.add(tile);
      if (!isBlack && count === 2) nextBlackTiles.add(tile);
    }
    blackTiles = nextBlackTiles;
  }

  return blackTiles.size;
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

console.log('part1:', part1(input));
console.log('part2:', part2(input));
