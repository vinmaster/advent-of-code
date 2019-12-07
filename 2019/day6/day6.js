const fs = require('fs');
const path = require('path');

function getEdgeList(input) {
  const reversedEdgeList = input
    .trim()
    .split('\n');
  return edgeList = reversedEdgeList.map(e => {
    const [b, a] = e.split(')');
    return [a, b];
  }).reduce((map, pair) => {
    map[pair[0]] = pair[1];
    return map;
  }, {});
}

function getPathToCOM(starting, edgeList) {
  const path = [starting];
  let other = starting;
  while (other !== 'COM') {
    other = edgeList[other];
    path.push(other);
  }
  return path;
}

function part1(input) {
  const edgeList = getEdgeList(input);
  const vertices = [...new Set(Object.keys(edgeList))];
  // All inputs doesn't have 2 orbit the same planet, direct orbits = vertices.length
  const directOrbits = vertices.length;
  let indirectOrbits = 0;

  for (const v of vertices) {
    let other = edgeList[v]; // Self is not indirect orbit
    let verticeIndirectOrbits = 0;

    while (other !== 'COM') {
      other = edgeList[other];
      verticeIndirectOrbits += 1;
    }
    indirectOrbits += verticeIndirectOrbits;
  }
  
  return directOrbits + indirectOrbits;
}

function part2(input) {
  const edgeList = getEdgeList(input);

  const youOrbit = edgeList['YOU'];
  const sanOrbit = edgeList['SAN'];
  
  const youPathToCOM = getPathToCOM(youOrbit, edgeList);
  const sanPathToCOM = getPathToCOM(sanOrbit, edgeList);
  
  while (true) {
    if (youPathToCOM.pop() !== sanPathToCOM.pop()) {
      break;
    }
  }
  
  // Add 2 for the planets popped after matching
  return youPathToCOM.length + sanPathToCOM.length + 2;
}

// Ways to implement graphs: edge list, adjacency matrix, adjacency lists
const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day6 part1:', part1(input));
console.log('day6 part2:', part2(input));
