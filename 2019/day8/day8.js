const fs = require('fs');
const path = require('path');

function countStr(str, target) {
  return (str.match(new RegExp(target, 'g')) || []).length;
}

function countLayerStr(layer, str) {
  let layerStr = '';
  for (const row of layer) {
    layerStr += row.join('');
  }
  return countStr(layerStr, str);
}

function decodeLayers(digits, width, height) {
  const size = width * height;
  const layers = [];

  for (let i = 0; i < digits.length / size; i++) {
    const layer = Array(height).fill().map(() => Array(width).fill(null));
    const nextLayerIndex = i * size;
    for (let y = 0; y < height; y++) {
      for (let x = 0; x < width; x++) {
        layer[y][x] = digits[nextLayerIndex + ((y * width) + x)];
      }
    }
    layers.push(layer);
  }
  return layers;
}

function mergeLayers(layers, width, height) {
  const finalLayer = Array(height).fill().map(() => Array(width).fill(null));

  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      for (let layer of layers) {
        if (finalLayer[y][x] === null) {
          finalLayer[y][x] = layer[y][x];
        }
        if (layer[y][x] !== 2) {
          finalLayer[y][x] = layer[y][x];
          break;
        }
      }
    }
  }
  return finalLayer;
}

function printLayer(layer, width, height) {
  let output = '';
  for (let row of layer) {
    row = row.join('')
      .replace(new RegExp('0', 'g'), ' ')
      .replace(new RegExp('1', 'g'), '#')
    output += row + '\n';
  }
  return output;
}

function part1(input) {
  const digits = input
    .trim()
    .split('')
    .map(s => parseInt(s, 10));
  
  const layers = decodeLayers(digits, 25, 6);
  let targetLayerIndex = null;
  let minZeroesCount = null;

  for (let i = 0; i < layers.length; i++) {
    const zeroesCount = countLayerStr(layers[i], '0');
    if (minZeroesCount === null || zeroesCount < minZeroesCount) {
      minZeroesCount = zeroesCount;
      targetLayerIndex = i;
    }
  }

  return countLayerStr(layers[targetLayerIndex], '1') * countLayerStr(layers[targetLayerIndex], '2');
}

function part2(input) {
  const digits = input
    .trim()
    .split('')
    .map(s => parseInt(s, 10));

  const layers = decodeLayers(digits, 25, 6);
  const layer = mergeLayers(layers, 25, 6);
  return '\n' + printLayer(layer, 25, 6);
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day8 part1:', part1(input));
console.log('day8 part2:', part2(input));
