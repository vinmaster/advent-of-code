type Pos = [number, number];

// A simple Min-Heap Priority Queue
class PriorityQueue<T> {
  private heap: [number, T][] = [];
  private parent = (i: number) => Math.floor((i - 1) / 2);
  private left = (i: number) => 2 * i + 1;
  private right = (i: number) => 2 * i + 2;
  private swap = (i: number, j: number) => {
    [this.heap[i], this.heap[j]] = [this.heap[j], this.heap[i]];
  };

  isEmpty = () => this.heap.length === 0;
  size = () => this.heap.length;

  push(priority: number, value: T) {
    this.heap.push([priority, value]);
    this.bubbleUp(this.heap.length - 1);
  }

  /** Remove and return the element with the lowest priority (smallest number) */
  pop(): [number, T] | undefined {
    if (this.isEmpty()) return undefined;
    this.swap(0, this.heap.length - 1);
    const min = this.heap.pop();
    if (!this.isEmpty()) {
      this.bubbleDown(0);
    }
    return min;
  }

  private bubbleUp(index: number) {
    while (index > 0 && this.heap[index][0] < this.heap[this.parent(index)][0]) {
      this.swap(index, this.parent(index));
      index = this.parent(index);
    }
  }

  private bubbleDown(index: number) {
    let smallest = index;
    const l = this.left(index);
    const r = this.right(index);
    const len = this.heap.length;
    if (l < len && this.heap[l][0] < this.heap[smallest][0]) {
      smallest = l;
    }
    if (r < len && this.heap[r][0] < this.heap[smallest][0]) {
      smallest = r;
    }
    if (smallest !== index) {
      this.swap(index, smallest);
      this.bubbleDown(smallest);
    }
  }
}

function parseInput(input: string) {
  const lines = input.trim().split('\n');
  const graph: Record<string, string> = {};
  for (let row = 0; row < lines.length; row++) {
    for (let col = 0; col < lines[row].length; col++) {
      graph[`${row},${col}`] = lines[row][col];
    }
  }
  return { graph, width: lines[0].length, height: lines.length, lines };
}

interface PathInfo {
  key: string;
  dist: number;
  requiredDoors: number; // bitmask
}

function buildKeyGraph(graph: Record<string, string>, starts: Pos[]) {
  const nodes: Record<string, Record<string, PathInfo>> = {};

  const keyToBit = (k: string) => 1 << (k.charCodeAt(0) - 97);
  const doorToBit = (d: string) => 1 << (d.charCodeAt(0) - 65);

  const bfsFrom = (startKey: string, startPos: Pos) => {
    const queue: [number, number, number, number][] = []; // y, x, dist, requiredDoors
    const visited = new Set<string>();
    queue.push([startPos[0], startPos[1], 0, 0]);
    visited.add(`${startPos[0]},${startPos[1]}`);
    const result: Record<string, PathInfo> = {};

    while (queue.length) {
      const [y, x, dist, doors] = queue.shift()!;
      const cell = graph[`${y},${x}`];

      // Found a key
      if (cell >= 'a' && cell <= 'z' && cell !== startKey) {
        result[cell] = { key: cell, dist, requiredDoors: doors };
      }

      const dirs = [
        [1, 0],
        [-1, 0],
        [0, 1],
        [0, -1],
      ];

      for (const [dy, dx] of dirs) {
        const ny = y + dy;
        const nx = x + dx;
        const next = graph[`${ny},${nx}`];
        if (!next || next === '#' || visited.has(`${ny},${nx}`)) continue;

        let newDoors = doors;
        if (next >= 'A' && next <= 'Z') newDoors |= doorToBit(next);
        visited.add(`${ny},${nx}`);
        queue.push([ny, nx, dist + 1, newDoors]);
      }
    }

    return result;
  };

  // Collect all keys and start nodes
  const keys: Record<string, Pos> = {};
  for (const k in graph) {
    const cell = graph[k];
    if (cell >= 'a' && cell <= 'z') {
      const [y, x] = k.split(',').map(Number);
      keys[cell] = [y, x];
    }
  }

  // Build graph for all keys
  for (const k in keys) nodes[k] = bfsFrom(k, keys[k]);

  // Build graph for all starts as '@0', '@1', ...
  starts.forEach((pos, i) => {
    nodes[`@${i}`] = bfsFrom(`@${i}`, pos);
  });

  return nodes;
}

function shortestPathToCollectAllKeys(graph: Record<string, string>, starts: Pos[]) {
  // Precompute key relationships
  const keyGraph = buildKeyGraph(graph, starts);

  const totalKeys = Object.keys(keyGraph).filter(
    k => k.length === 1 && k >= 'a' && k <= 'z'
  ).length;
  const allKeysMask = (1 << totalKeys) - 1;

  const keyIndex = (k: string) => k.charCodeAt(0) - 97;
  const keyBit = (k: string) => 1 << keyIndex(k);

  type State = { bots: string[]; keys: number };
  const visited = new Map<string, number>();

  const pq = new PriorityQueue<State>();
  const push = (steps: number, bots: string[], keys: number) => {
    const key = `${bots.join(',')}|${keys}`;
    if (visited.has(key) && visited.get(key)! <= steps) return;
    visited.set(key, steps);

    pq.push(steps, { bots, keys });
  };

  const startBots = starts.map((_, i) => `@${i}`);
  push(0, startBots, 0);

  while (!pq.isEmpty()) {
    const popped = pq.pop()!;
    if (!popped) break;

    const [steps, state] = popped;
    if (state.keys === allKeysMask) return steps;
    for (let i = 0; i < state.bots.length; i++) {
      const bot = state.bots[i];

      if (!keyGraph[bot]) continue;
      for (const targetKey in keyGraph[bot]) {
        const path = keyGraph[bot][targetKey];
        const bit = keyBit(targetKey);
        if (state.keys & bit) continue; // already have this key
        if ((path.requiredDoors & ~state.keys) !== 0) continue; // missing doors
        const newKeys = state.keys | bit;
        const newBots = state.bots.slice();
        newBots[i] = targetKey;
        push(steps + path.dist, newBots, newKeys);
      }
    }
  }

  return -1;
}

function part1(input: string) {
  const { graph, height, width } = parseInput(input);

  let start: Pos = [0, 0];
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      if (graph[`${y},${x}`] === '@') start = [y, x];
    }
  }

  const result = shortestPathToCollectAllKeys(graph, [start]);
  console.log('part1:', result);
}

function part2(input: string) {
  const { graph, height, width } = parseInput(input);

  // Modify map to create 4 robots
  let start: Pos = [0, 0];
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      if (graph[`${y},${x}`] === '@') start = [y, x];
    }
  }
  const [sy, sx] = start;
  const starts: Pos[] = [
    [sy - 1, sx - 1],
    [sy - 1, sx + 1],
    [sy + 1, sx - 1],
    [sy + 1, sx + 1],
  ];
  graph[`${sy},${sx}`] = '#';
  graph[`${sy - 1},${sx}`] = '#';
  graph[`${sy + 1},${sx}`] = '#';
  graph[`${sy},${sx - 1}`] = '#';
  graph[`${sy},${sx + 1}`] = '#';
  for (const [y, x] of starts) graph[`${y},${x}`] = '@';

  const result = shortestPathToCollectAllKeys(graph, starts);
  console.log('part2:', result);
}

const input = await Bun.file(`${import.meta.dir}/input.txt`).text();
part1(input);
part2(input);
