type Input = string[][];
type Coord = [number, number];
const DIRS = ['^', '>', 'v', '<'] as const;
type Direction = (typeof DIRS)[number];
const DIRS_DELTA: Record<Direction, Coord> = {
  '^': [-1, 0],
  '>': [0, 1],
  v: [1, 0],
  '<': [0, -1],
};

class HeapNode {
  val: any;
  priority: number;
  path?: any;
  constructor(val: any, priority: number, path?: any) {
    this.val = val;
    this.priority = priority;
    this.path = path;
  }
}

// Minheap
class PriorityQueue {
  pq: HeapNode[];
  comparator: (a: any, b: any) => number;
  /**
   * @template T
   * @typedef {function(T, T): number} Comparator
   */

  /**
   * @param {Comparator<T>} [comparator=(a, b) => a - b]
   */
  constructor(comparator = (a, b) => a - b) {
    this.pq = [];
    this.comparator = comparator;
  }

  push(value, priority = 0, path?) {
    this.pq.push(new HeapNode(value, priority, path));
    this.heapifyUp();
  }

  pop() {
    if (this.isEmpty()) return null;
    if (this.pq.length === 1) return this.pq.pop() as HeapNode;
    const root = this.pq[0];
    this.pq[0] = this.pq.pop() as HeapNode;
    this.heapifyDown();
    return root;
  }

  size() {
    return this.pq.length;
  }

  isEmpty() {
    return this.pq.length === 0;
  }

  peek() {
    return this.isEmpty() ? null : this.pq[0];
  }

  pushPop(value) {
    if (this.isEmpty()) {
      this.pq.push(value);
      return null;
    }
    const root = this.pq[0];
    this.pq[0] = value;
    this.heapifyDown();
    return root;
  }

  heapifyUp() {
    let currentIndex = this.pq.length - 1;
    while (currentIndex > 0) {
      const parentIndex = (currentIndex - 1) >> 1;
      if (this.comparator(this.pq[currentIndex].priority, this.pq[parentIndex].priority) < 0) {
        this.swap(currentIndex, parentIndex);
        currentIndex = parentIndex;
      } else break;
    }
  }

  heapifyDown() {
    let currentIndex = 0;
    while (true) {
      const leftChildIndex = (currentIndex << 1) + 1;
      const rightChildIndex = (currentIndex << 1) + 2;
      let smallestChildIndex = currentIndex;

      if (
        leftChildIndex < this.pq.length &&
        this.comparator(this.pq[leftChildIndex].priority, this.pq[smallestChildIndex].priority) < 0
      ) {
        smallestChildIndex = leftChildIndex;
      }
      if (
        rightChildIndex < this.pq.length &&
        this.comparator(this.pq[rightChildIndex].priority, this.pq[smallestChildIndex].priority) < 0
      ) {
        smallestChildIndex = rightChildIndex;
      }
      if (currentIndex !== smallestChildIndex) {
        this.swap(currentIndex, smallestChildIndex);
        currentIndex = smallestChildIndex;
      } else break;
    }
  }

  swap(i, j) {
    [this.pq[i], this.pq[j]] = [this.pq[j], this.pq[i]];
  }
}

function opposite(dir: Direction): Direction {
  return DIRS[(DIRS.indexOf(dir) + 2) % DIRS.length];
}

function moveCoord(coord: Coord, dir: Direction): Coord {
  let [dr, dc] = DIRS_DELTA[dir];
  return [coord[0] + dr, coord[1] + dc];
}

function isValid(grid: Input, coord: Coord): boolean {
  return coord[0] >= 0 && coord[0] < grid.length && coord[1] >= 0 && coord[1] < grid[0].length;
}

function findCell(grid: Input, value: string) {
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      if (grid[row][col] === value) return [row, col] as Coord;
    }
  }
  return null;
}

function dijkstra(grid: Input, starts: [Coord, Direction][], end: Coord, earlyReturn = true) {
  let unvisited = new PriorityQueue();
  let distances = new Map<string, number>();
  for (let [start, dir] of starts) {
    let startingKey = `${start.toString()},${dir}`;
    unvisited.push(startingKey, 0, [start.toString()]);
    distances.set(startingKey, 0);
  }

  while (!unvisited.isEmpty()) {
    let current = unvisited.pop()!;
    let currentCoord = current.val.split(',').slice(0, 2).map(Number);
    let currentDir = current.val.split(',')[2];
    // If a shortest distance has been found this nodes distance is greater then skip continuing to process it.
    if (distances.get(current.val)! < current.priority) continue;
    if (currentCoord.toString() === end.toString()) {
      if (earlyReturn) return current.priority;
      continue;
    }
    for (let d of DIRS) {
      if (d === opposite(currentDir)) continue;
      let next = d === currentDir ? moveCoord(currentCoord, d) : currentCoord;
      let nextKey = `${next.toString()},${d}`;
      let cost = (d === currentDir ? 1 : 1000) + current.priority;

      if (!isValid(grid, next) || grid[currentCoord[0]][currentCoord[1]] === '#') continue;

      if (!distances.has(nextKey) || cost < distances.get(nextKey)!) {
        distances.set(nextKey, cost);
        unvisited.push(nextKey, cost, [...current.path, next.toString()]);
      }
    }
  }
  return distances;
}

export function part1(input: Input) {
  let start = findCell(input, 'S')!;
  let end = findCell(input, 'E')!;
  return dijkstra(input, [[start, '>']], end);
}

export function part2(input: Input) {
  let start = findCell(input, 'S')!;
  let end = findCell(input, 'E')!;

  let unvisited = new PriorityQueue();
  let startingKey = `${start.toString()},>`;
  unvisited.push(startingKey, 0, [start.toString()]);
  // Finding optimal paths
  let minCost = Infinity;
  let optimalPaths = [] as { path; cost }[];
  let visited = new Set<string>();

  while (!unvisited.isEmpty()) {
    let current = unvisited.pop()!;
    let currentCoord = current.val.split(',').slice(0, 2).map(Number);
    let currentDir = current.val.split(',')[2];
    visited.add(current.val);
    if (currentCoord.toString() === end.toString()) {
      if (current.priority < minCost) {
        minCost = current.priority;
        optimalPaths = [{ path: current.path, cost: current.priority }];
      } else if (current.priority === minCost) {
        optimalPaths.push({ path: current.path, cost: current.priority });
      }
      continue;
    }
    for (let d of DIRS) {
      if (d === opposite(currentDir)) continue;
      let next = d === currentDir ? moveCoord(currentCoord, d) : currentCoord;
      if (!isValid(input, next) || input[currentCoord[0]][currentCoord[1]] === '#') continue;
      let nextKey = `${next.toString()},${d}`;
      if (visited.has(nextKey)) continue;
      let cost = (d === currentDir ? 1 : 1000) + current.priority;
      unvisited.push(nextKey, cost, [...current.path, next.toString()]);
    }
  }
  let visitedPlaces = new Set();
  optimalPaths.forEach(optPath => {
    optPath.path.forEach(cell => visitedPlaces.add(cell));
  });
  return visitedPlaces.size;

  // Tried to match optimal distance from 2 ends
  // let fromStart = dijkstra(input, [[start, '>']], end, false) as Map<string, number>;
  // let fromEnd = dijkstra(
  //   input,
  //   DIRS.map(d => [end, d]),
  //   start,
  //   false
  // ) as Map<string, number>;
  // let optimal = part1(input);
  // let visited = new Set<string>();
  // for (let row = 0; row < input.length; row++) {
  //   for (let col = 0; col < input[0].length; col++) {
  //     for (let d of DIRS) {
  //       let stateFromStart = `${row},${col},${d}`;
  //       let stateFromEnd = `${row},${col},${opposite(d as Direction)}`;
  //       if (fromStart.has(stateFromStart) && fromEnd.has(stateFromEnd)) {
  //         let startDistance = fromStart.get(stateFromStart)!;
  //         let endDistance = fromEnd.get(stateFromEnd)!;
  //         if (startDistance + endDistance === optimal) {
  //           visited.add([row, col].toString());
  //         }
  //       }
  //     }
  //   }
  // }
  // return visited.size;
}

function parseInput(inputString: string): Input {
  return inputString
    .trim()
    .split('\n')
    .map(line => line.split(''));
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
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
`;

  testInput = `
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
