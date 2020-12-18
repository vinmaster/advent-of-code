const fs = require('fs');
const path = require('path');
const util = require('util');

// util.inspect.defaultOptions.maxArrayLength = null;
// util.inspect.defaultOptions.showHidden = true;
// util.inspect.defaultOptions.depth = null;
// util.inspect.defaultOptions.compact = true;

const log = (...args) => console.log(...args);
const toCoord = (x, y, z) => `${x},${y},${z}`;
const toCoordInt = (coord) => coord.split(',').map(Number);
const getCoord = (coords, x, y, z) => coords[toCoord(x, y, z)];
const setCoord = (coords, x, y, z, value) => coords[toCoord(x, y, z)] = value;

const toCoord4 = (x, y, z, w) => `${x},${y},${z},${w}`;
const getCoord4 = (coords, x, y, z, w) => coords[toCoord4(x, y, z, w)];
const setCoord4 = (coords, x, y, z, w, value) => coords[toCoord4(x, y, z, w)] = value;

const ACTIVE = '#';
const INACTIVE = '.';

// /** @typedef {{ ACTIVE: string, INACTIVE: string }} TYPES */
/**
 * @typedef TYPES
 * @property {string} ACTIVE
 * @property {string} INACTIVE
 */
const TYPES = {
  ACTIVE,
  INACTIVE,
}

const DV = [
  // z = 0
  [-1, -1, 0], [0, -1, 0], [1, -1, 0], [-1, 0, 0],
  [1, 0, 0], [-1, 1, 0], [0, 1, 0], [1, 1, 0],
  // z = 1
  [-1, -1, 1], [0, -1, 1], [1, -1, 1],
  [-1, 0, 1], [0, 0, 1], [1, 0, 1],
  [-1, 1, 1], [0, 1, 1], [1, 1, 1],
  // z = -1
  [-1, -1, -1], [0, -1, -1], [1, -1, -1],
  [-1, 0, -1], [0, 0, -1], [1, 0, -1],
  [-1, 1, -1], [0, 1, -1], [1, 1, -1],
];

let DV4 = [];
for (let w = -1; w <= 1; w++) {
  for (let z = -1; z <= 1; z++) {
    for (let y = -1; y <= 1; y++) {
      for (let x = -1; x <= 1; x++) {
        if (x === 0 && y === 0 && z === 0 && w === 0) continue;
        DV4.push([x, y, z, w]);
      }
    }
  }
}

function getNeighbors(coords, x, y, z) {
  return DV.map(([dx, dy, dz]) => {
    return coords[toCoord(
      x + dx,
      y + dy,
      z + dz,
    )];
  })
}

/**
 * Get neighbors in 4 dimensional space
 * @param {Object<string, string>} coords - Object containing coords
 * @param {number} x - x dimension
 * @param {number} y - y dimension
 * @param {number} z - z dimension
 * @param {number} w - w dimension
 * @returns {string[]} List of strings
 */
function getNeighbors4(coords, x, y, z, w) {
  return DV4.map(([dx, dy, dz, dw]) => {
    return coords[toCoord4(
      x + dx,
      y + dy,
      z + dz,
      w + dw,
    )];
  })
}

function simulateCycles(cycles, coords) {
  for (let c = 0; c < cycles; c++) {
    let nextCoords = {};

    let keys = Object.keys(coords);
    let xs = keys.map(coord => toCoordInt(coord)[0]);
    let ys = keys.map(coord => toCoordInt(coord)[1]);
    let zs = keys.map(coord => toCoordInt(coord)[2]);
    let minX = Math.min(...xs);
    let maxX = Math.max(...xs);
    let minY = Math.min(...ys);
    let maxY = Math.max(...ys);
    let minZ = Math.min(...zs);
    let maxZ = Math.max(...zs);

    for (let z = minZ - 1; z <= maxZ + 1; z++) {
      for (let y = minY - 1; y <= maxY + 1; y++) {
        for (let x = minX - 1; x <= maxX + 1; x++) {
          let neighbors = getNeighbors(coords, x, y, z);
          let actives = neighbors.filter(n => n === ACTIVE).length;
          let current = getCoord(coords, x, y, z);

          if (current === ACTIVE && ![2, 3].includes(actives)) {
            setCoord(nextCoords, x, y, z, INACTIVE);
          }
          if ([INACTIVE, undefined].includes(current) && actives === 3) {
            setCoord(nextCoords, x, y, z, ACTIVE);
          }
        }
      }
    }
    Object.keys(coords).map(coord => {
      if (!nextCoords[coord]) nextCoords[coord] = coords[coord];
    })
    coords = nextCoords;
  }

  return coords;
}

function simulateCycles4(cycles, coords) {
  for (let c = 0; c < cycles; c++) {
    let nextCoords = {};

    let keys = Object.keys(coords);
    let xs = keys.map(coord => toCoordInt(coord)[0]);
    let ys = keys.map(coord => toCoordInt(coord)[1]);
    let zs = keys.map(coord => toCoordInt(coord)[2]);
    let ws = keys.map(coord => toCoordInt(coord)[3]);
    let minX = Math.min(...xs);
    let maxX = Math.max(...xs);
    let minY = Math.min(...ys);
    let maxY = Math.max(...ys);
    let minZ = Math.min(...zs);
    let maxZ = Math.max(...zs);
    let minW = Math.min(...ws);
    let maxW = Math.max(...ws);

    for (let w = minW - 1; w <= maxW + 1; w++) {
      for (let z = minZ - 1; z <= maxZ + 1; z++) {
        for (let y = minY - 1; y <= maxY + 1; y++) {
          for (let x = minX - 1; x <= maxX + 1; x++) {
            let neighbors = getNeighbors4(coords, x, y, z, w);
            let actives = neighbors.filter(n => n === ACTIVE).length;
            let current = getCoord4(coords, x, y, z, w);

            if (current === ACTIVE && ![2, 3].includes(actives)) {
              setCoord4(nextCoords, x, y, z, w, INACTIVE);
            }
            if ([INACTIVE, undefined].includes(current) && actives === 3) {
              setCoord4(nextCoords, x, y, z, w, ACTIVE);
            }
          }
        }
      }
    }
    Object.keys(coords).map(coord => {
      if (!nextCoords[coord]) nextCoords[coord] = coords[coord];
    })
    coords = nextCoords;
  }

  return coords;
}

function printCoords(coords) {
  let keys = Object.keys(coords);
  let xs = keys.map(coord => toCoordInt(coord)[0]);
  let ys = keys.map(coord => toCoordInt(coord)[1]);
  let zs = keys.map(coord => toCoordInt(coord)[2]);
  let minX = Math.min(...xs);
  let maxX = Math.max(...xs);
  let minY = Math.min(...ys);
  let maxY = Math.max(...ys);
  let minZ = Math.min(...zs);
  let maxZ = Math.max(...zs);

  for (let z = minZ; z <= maxZ; z++) {
    let layer = '';
    for (let y = minY; y <= maxY; y++) {
      for (let x = minX; x <= maxX; x++) {
        let value = getCoord(coords, x, y, z);
        value = value === ACTIVE ? value : '.';
        layer += value;
      }
      layer += '\n';
    }
    log(layer);
  }
}

function part1(input) {
  let lines = input
    .trim()
    .split('\n');

  let coords = [];
  for (let y = 0; y < lines.length; y++) {
    let line = lines[y];
    for (let x = 0; x < lines[y].length; x++) {
      setCoord(coords, x, y, 0, line[x]);
    }
  }

  coords = simulateCycles(6, coords);
  return Object.keys(coords).filter(c => coords[c] === ACTIVE).length;
}

function part2(input) {
  let lines = input
    .trim()
    .split('\n');

  /** @type {Object<4D, string>} */
  let coords = [];
  for (let y = 0; y < lines.length; y++) {
    let line = lines[y];
    for (let x = 0; x < lines[y].length; x++) {
      setCoord4(coords, x, y, 0, 0, line[x]);
    }
  }

  coords = simulateCycles4(6, coords);
  return Object.keys(coords).filter(c => coords[c] === ACTIVE).length;
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

log('day17 part1:', part1(input));
log('day17 part2:', part2(input));
