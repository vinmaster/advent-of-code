const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
// util.inspect.defaultOptions.showHidden = true;
// util.inspect.defaultOptions.depth = null;
// util.inspect.defaultOptions.compact = true;

let BORDER_DIRS = ['top', 'bottom', 'left', 'right'];

/**
 * @template T
 * @param {T[][]} array 
 * @returns {T[][]}
 */
function rotate(array) {
  return array[0].map((_, colIndex) => array.map(row => row[colIndex]));
}

/**
 * @template T
 * @param {T[][]} array 
 * @returns {T[][]}
 */
function flipVertical(array) {
  let copy = array.map(a => a.slice());
  return copy.reverse();
}

/**
 * @param {string} entry
 * @returns {{ id: string, tile: string[][]}}
 */
function parseTile(entry) {
  let [idStr, ...lines] = entry.split('\n');
  let [_, id] = idStr.match(/^Tile (\d+):$/);

  let tile = [];
  for (let y = 0; y < lines.length; y++) {
    tile.push([]);
    for (let x = 0; x < lines[y].length; x++) {
      tile[y].push(lines[y][x]);
    }
  }
  return { id, tile };
}

/**
 * @param {string[][]} array
 * @param {'top' | 'bottom' | 'left' | 'right'} border
 * @returns {string}
 */
function grabBorder(array, border) {
  if (border === 'top') {
    return array[0].join('');
  } else if (border === 'bottom') {
    return array[array.length - 1].join('');
  } else if (border === 'left') {
    return array.reduce((str, a) => str + a[0], '');
  } else if (border === 'right') {
    return array.reduce((str, a) => str + a[a.length - 1], '');
  }
}

/**
 * @template T
 * @param {T[]} arr1 
 * @param {T[]} arr2 
 */
function intersection(arr1, arr2) {
  return arr1.filter(x => arr2.includes(x));
}

/**
 * @param {string} id 
 * @param {string[][]} tilesMap 
 * @returns {string[]}
 */
function matchingBorderIds(targetId, tilesMap) {
  let ids = [];
  let targetBorders = BORDER_DIRS
    .map(b => grabBorder(tilesMap[targetId], b));
  let reverseTargetBorders = [...targetBorders].reverse();
  targetBorders = targetBorders.concat(reverseTargetBorders);
  for (let id of Object.keys(tilesMap)) {
    if (id !== targetId) {
      let borders = BORDER_DIRS
        .map(b => grabBorder(tilesMap[id], b));
      let reverseBorders = [...borders].reverse();
      borders = borders.concat(reverseBorders);
      let matching = intersection(targetBorders, borders);
      if (matching.length >= 2) {
        ids.push(id);
      }
    }
  }
  return ids;
}

/** @param {string[][]} tile */
function printTile(tile) {
  return tile.map(a => a.join('')).join('\n')
}

function part1(input) {
  /** @type {[string string]} */
  let tileEntries = input
    .trim()
    .split('\n\n');

  let tilesMap = {};

  // Parse tiles into 2d array
  tileEntries.reduce((map, entry) => {
    let { id, tile } = parseTile(entry);
    map[id] = tile;
    return map;
  }, tilesMap);

  // let targetBorders = BORDER_DIRS
  //   .map(b => grabBorder(tilesMap['2311'], b));
  // let reverseTargetBorders = [...targetBorders].reverse();
  // targetBorders = targetBorders.concat(reverseTargetBorders);
  // let targetBorders2 = BORDER_DIRS
  //   .map(b => grabBorder(tilesMap['1951'], b));
  // let reverseTargetBorders2 = [...targetBorders2].reverse();
  // targetBorders2 = targetBorders2.concat(reverseTargetBorders2);
  // console.log(targetBorders);
  // console.log(targetBorders2);
  // console.log(intersection(targetBorders, targetBorders2));

  for (let id of Object.keys(tilesMap)) {
    let ids = matchingBorderIds(id, tilesMap);
    if (ids.length !== 0) {
      console.log(id, ids);
    }
  }

  // console.log(printTile(tilesMap['1009']));
  // console.log(grabBorder(tilesMap['1009'], 'right'));

  // console.log(parseTile(entry));
}

function part2(input) {
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

input = `
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
`

console.log('day20 part1:', part1(input));
console.log('day20 part2:', part2(input));
