function part1(input: string) {
  let map = input
    .trim()
    .split('\n')
    .map(line => line.split('').map(Number));
  let start: [number, number] = [0, 0];
  let end: [number, number] = [map.length - 1, map[0].length - 1];
  let cost = dijkstra(map, start, end);
  return cost;
}

function part2(input: string) {
  let map = input
    .trim()
    .split('\n')
    .map(line => line.split('').map(Number));
  map = growMap(map, 5);
  let start: [number, number] = [0, 0];
  let end: [number, number] = [map.length - 1, map[0].length - 1];
  let cost = dijkstra(map, start, end);
  return cost;
}

function dijkstra(map: number[][], start: [number, number], end: [number, number]) {
  let queue: [number, number][] = [];
  queue.push(start);
  let costs: Record<string, number> = { [start.toString()]: 0 };
  let getNeighbors = ([row, col]: [number, number]) => {
    let dirs = [
      [-1, 0],
      [0, -1],
      [0, 1],
      [1, 0],
    ];
    return dirs
      .map(([dr, dc]) => [row + dr, col + dc] as [number, number])
      .filter(([r, c]) => r >= 0 && c >= 0 && r < map.length && c < map[0].length);
  };
  let getCostToCoord = (current, coord) => map[coord[0]][coord[1]];
  while (queue.length > 0) {
    let current = queue.shift()!;
    for (let next of getNeighbors(current)) {
      let newCost = costs[current.toString()] + getCostToCoord(current, next);
      if (costs[next.toString()] === undefined || newCost < costs[next.toString()]) {
        costs[next.toString()] = newCost;
        queue.push(next);
      }
    }
  }
  return costs[end.toString()];
}

function growMap(map: number[][], multiply: number): number[][] {
  let newMap: number[][] = JSON.parse(JSON.stringify(map));
  let increaseRisk = n => ((n + 1) % 10 === 0 ? 1 : (n + 1) % 10);
  for (let row = 0; row < multiply; row++) {
    for (let col = 0; col < multiply; col++) {
      if (col === 0) continue;

      for (let r = 0; r < map.length; r++) {
        // Take slice on the left
        let newNums = newMap[row * map.length + r]
          .slice((col - 1) * map[0].length, (col - 1) * map[0].length + map[0].length)
          .map(increaseRisk);
        newMap[row * map.length + r].push(...newNums);

        // Transpose it to first column in later rows
        if (row === 0) {
          newMap.push([]);
          newMap[col * map.length + r].push(...newNums);
        }
      }
    }
  }
  return newMap;
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
