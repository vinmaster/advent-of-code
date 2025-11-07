import { readFileSync } from 'fs';
import { resolve } from 'path';

const BORDER_DIRS = ['top', 'bottom', 'left', 'right'];

const rotate = a => a[0].map((_, i) => a.map(r => r[i]).reverse());
const flipH = a => a.map(r => [...r].reverse());
const flipV = a => [...a].reverse();

const grabBorder = (a, dir) => {
  if (dir === 'top') return a[0].join('');
  if (dir === 'bottom') return a[a.length - 1].join('');
  if (dir === 'left') return a.map(r => r[0]).join('');
  if (dir === 'right') return a.map(r => r[r.length - 1]).join('');
};

// Generate all 8 orientations (4 rotations × flip)
const orientations = tile => {
  const variants = [];
  let t = tile;
  for (let i = 0; i < 4; i++) {
    variants.push(t);
    variants.push(flipH(t));
    t = rotate(t);
  }
  return variants;
};

/**
 * Parse a tile block into { id, tile }
 */
function parseTile(entry) {
  const [idStr, ...lines] = entry.trim().split('\n');
  const id = idStr.match(/^Tile (\d+):$/)[1];
  const tile = lines.map(line => line.split(''));
  return { id, tile };
}

/**
 * Find tiles that share borders with targetId
 */
function matchingBorderIds(targetId, tilesMap) {
  const ids = [];
  const targetTile = tilesMap[targetId];

  const targetBorders = BORDER_DIRS.flatMap(b => {
    const border = grabBorder(targetTile, b);
    return [border, border.split('').reverse().join('')];
  });

  for (const [id, tile] of Object.entries(tilesMap)) {
    if (id === targetId) continue;

    const borders = BORDER_DIRS.flatMap(b => {
      const border = grabBorder(tile, b);
      return [border, border.split('').reverse().join('')];
    });

    const match = borders.some(b => targetBorders.includes(b));
    if (match) ids.push(id);
  }

  return ids;
}

function parseTile(entry) {
  const [idLine, ...lines] = entry.trim().split('\n');
  const id = idLine.match(/^Tile (\d+):$/)[1];
  return { id: Number(id), tile: lines.map(l => l.split('')) };
}

function getBorders(tile) {
  return BORDER_DIRS.map(d => grabBorder(tile, d));
}

function findMatches(tiles) {
  const matches = new Map();
  for (const [id, tile] of tiles) {
    const borders = getBorders(tile);
    const variants = borders.concat(borders.map(b => [...b].reverse().join('')));
    const matched = [];
    for (const [oid, otile] of tiles) {
      if (id === oid) continue;
      const oborders = getBorders(otile);
      const ovar = oborders.concat(oborders.map(b => [...b].reverse().join('')));
      if (variants.some(b => ovar.includes(b))) matched.push(oid);
    }
    matches.set(id, matched);
  }
  return matches;
}

function assemble(tiles, matches) {
  const size = Math.sqrt(tiles.size);
  const placed = new Map(); // [y,x] -> {id,tile}
  const used = new Set();

  // pick a corner
  const cornerId = [...matches.entries()].find(([_, v]) => v.length === 2)[0];
  const cornerTile = tiles.get(cornerId);
  used.add(cornerId);

  // find orientation with unmatched borders on top/left
  let orientedCorner;
  for (const variant of orientations(cornerTile)) {
    const top = grabBorder(variant, 'top');
    const left = grabBorder(variant, 'left');
    const topMatches = [...tiles.entries()].some(([oid, ot]) => {
      if (oid === cornerId) return false;
      const b = getBorders(ot);
      const br = b.map(x => [...x].reverse().join(''));
      return b.concat(br).includes(top);
    });
    const leftMatches = [...tiles.entries()].some(([oid, ot]) => {
      if (oid === cornerId) return false;
      const b = getBorders(ot);
      const br = b.map(x => [...x].reverse().join(''));
      return b.concat(br).includes(left);
    });
    if (!topMatches && !leftMatches) {
      orientedCorner = variant;
      break;
    }
  }

  placed.set('0,0', { id: cornerId, tile: orientedCorner });

  for (let y = 0; y < size; y++) {
    for (let x = 0; x < size; x++) {
      if (x === 0 && y === 0) continue;

      let targetBorder, dir, matchDir;

      if (x > 0) {
        const leftTile = placed.get(`${y},${x - 1}`);
        targetBorder = grabBorder(leftTile.tile, 'right');
        dir = 'left';
        matchDir = 'right';
      } else {
        const topTile = placed.get(`${y - 1},${x}`);
        targetBorder = grabBorder(topTile.tile, 'bottom');
        dir = 'top';
        matchDir = 'bottom';
      }

      for (const [oid, otile] of tiles) {
        if (used.has(oid)) continue;

        for (const variant of orientations(otile)) {
          const border = grabBorder(variant, dir);
          if (border === targetBorder) {
            placed.set(`${y},${x}`, { id: oid, tile: variant });
            used.add(oid);
            y = y; // for clarity
            x = x;
            break;
          }
        }
        if (used.has(oid)) break;
      }
    }
  }

  return { placed, size };
}

function combineImage(placed, size) {
  const tileSize = placed.get('0,0').tile.length - 2;
  const image = [];

  for (let y = 0; y < size; y++) {
    for (let row = 1; row <= tileSize; row++) {
      let line = '';
      for (let x = 0; x < size; x++) {
        const tile = placed.get(`${y},${x}`).tile;
        line += tile[row].slice(1, -1).join('');
      }
      image.push(line.split(''));
    }
  }

  return image;
}

const monsterPattern = ['                  # ', '#    ##    ##    ###', ' #  #  #  #  #  #   '];
const monsterCoords = [];
for (let y = 0; y < monsterPattern.length; y++) {
  for (let x = 0; x < monsterPattern[y].length; x++) {
    if (monsterPattern[y][x] === '#') monsterCoords.push([x, y]);
  }
}

function countMonsters(image) {
  const variants = orientations(image);
  for (const variant of variants) {
    let count = 0;
    const imgH = variant.length;
    const imgW = variant[0].length;
    const marked = variant.map(r => [...r]);

    for (let y = 0; y < imgH - 3; y++) {
      for (let x = 0; x < imgW - 20; x++) {
        if (monsterCoords.every(([dx, dy]) => variant[y + dy][x + dx] === '#')) {
          count++;
          monsterCoords.forEach(([dx, dy]) => (marked[y + dy][x + dx] = 'O'));
        }
      }
    }

    if (count > 0) {
      const roughness = marked.flat().filter(c => c === '#').length;
      return { count, roughness };
    }
  }
  return { count: 0, roughness: 0 };
}

function part1(input) {
  const tiles = input.trim().split('\n\n').map(parseTile);
  const tilesMap = Object.fromEntries(tiles.map(t => [t.id, t.tile]));
  const neighbors = {};
  for (const id of Object.keys(tilesMap)) {
    neighbors[id] = matchingBorderIds(id, tilesMap);
  }
  const corners = Object.entries(neighbors)
    .filter(([_, ids]) => ids.length === 2)
    .map(([id]) => Number(id));
  const product = corners.reduce((a, b) => a * b, 1);
  return product;
}

function part2(input) {
  const tiles = new Map(
    input
      .trim()
      .split('\n\n')
      .map(t => {
        const parsed = parseTile(t);
        return [parsed.id, parsed.tile];
      })
  );
  const matches = findMatches(tiles);
  const { placed, size } = assemble(tiles, matches);
  const image = combineImage(placed, size);
  const { count, roughness } = countMonsters(image);
  return roughness;
}

// Use import.meta.dir in Bun instead of __dirname
const inputPath = resolve(import.meta.dir, 'input.txt');
const input = readFileSync(inputPath, 'utf8');

// Example test input
const exampleInput = `
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
