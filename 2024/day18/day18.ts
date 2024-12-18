type Input = Coord[];
type Grid = string[][];
type Coord = [number, number];
const DIRS = ['^', '>', 'v', '<'] as const;
type Direction = (typeof DIRS)[number];
const DIRS_DELTA: Record<Direction, Coord> = {
  '^': [-1, 0],
  '>': [0, 1],
  v: [1, 0],
  '<': [0, -1],
};
class PriorityQueue {
  heap: any[];
  constructor() {
    this.heap = [];
  }

  /**
   * Add a new item to the heap
   * @param {{y: number, x: number, direction: string}} val The node being queued
   * @param {number} priority Its weight in search
   * @param {string[]} path The path history to arrive at this node
   */
  add(val, priority, path) {
    // Create the new node for the heap
    let newNode = { val, priority, path };
    // Push the node on to the end of the heap
    this.heap.push(newNode);
    // Bubble up the node to the correct place
    this.#heapifyUp();
  }

  /**
   * Remove the next item from the heap. Since this is a min heap
   * it will always be the item with the lowest priority
   * @returns {{
   *  val: {y: number, x: number, direction: string},
   *  priority: number,
   *  path: string[]
   * }} The node with the lowest priority value
   */
  remove(): { val: Coord; priority: number; path: string[] } {
    // Get the head item from the heap
    const min = this.heap[0];
    // Pop the last item off the heap
    const end = this.heap.pop();
    // If there are still nodes in the heap then
    // add the end node to the front of the heap
    // and heapify down the new node at the top
    // of the heap
    if (this.heap.length > 0) {
      this.heap[0] = end;
      this.#heapifyDown();
    }
    return min;
  }

  /**
   * Return true if there are still nodes to evaluate
   * @returns {boolean} True if the queue has nodes in it
   */
  get hasNodes() {
    return this.heap.length > 0;
  }

  /**
   * Heapify up a new node to the correct place in the heap
   */
  #heapifyUp() {
    // Get the index of the new node that was just added
    let index = this.heap.length - 1;
    // Get the new node
    const current = this.heap[index];
    // Continue moving the index up the heap until it
    // breaks out or reaches the top
    while (index > 0) {
      // Parent index
      let parentIndex = Math.floor((index - 1) / 2);
      // Parent element
      let parent = this.heap[parentIndex];
      // If the current element has larger priority value than
      // it's parent then break out. It is in the correct place
      // in the heap
      if (current.priority >= parent.priority) break;
      // Otherwise swap places with the parent item
      this.#swap(index, parentIndex);
      // Set index to the elements new index which is the
      // parent's old index
      index = parentIndex;
    }
  }

  /**
   * Heapify down a node added to the top of the heap from the
   * bottom into it's correct place
   */
  #heapifyDown() {
    // Starting index of node to heapify down
    let index = 0;
    // Length of the heap
    const length = this.heap.length;
    // The node to move down the heap
    const current = this.heap[0];
    // Continue moving the node down until it breaks out when
    // it find the correct place for the node
    while (true) {
      // Left child index
      let leftChildIdx = 2 * index + 1;
      // Right child index
      let rightChildIdx = 2 * index + 2;
      let leftChild, rightChild;
      let swap: any = null;

      // If the left child index if valid
      if (leftChildIdx < length) {
        // Left child node
        leftChild = this.heap[leftChildIdx];
        // If the left child priority is less than the current
        // node then swap the current node with the left child
        if (leftChild.priority < current.priority) {
          swap = leftChildIdx;
        }
      }

      // If the right child index is valid
      if (rightChildIdx < length) {
        // Right child node
        rightChild = this.heap[rightChildIdx];
        // If there is not a swap and the right child priority
        // is less than the current priority or if there is a
        // swap and the right child priority is less than the left
        // child priority then swap the current with the right child
        if (
          (swap === null && rightChild.priority < current.priority) ||
          (swap !== null && rightChild.priority < leftChild.priority)
        ) {
          swap = rightChildIdx;
        }
      }

      // If there is no swap then break out of the loop since the
      // current node is now in the correct place in the heap
      if (swap === null) break;

      // Otherwise swap the child node with the current node
      this.#swap(index, swap);
      // Set the current index to the one it was swapped into
      index = swap;
    }
  }

  /**
   * Basic swap method
   * @param {number} index1 Node index 1
   * @param {number} index2 Node index 2
   */
  #swap(index1, index2) {
    [this.heap[index1], this.heap[index2]] = [this.heap[index2], this.heap[index1]];
  }
}

function printGrid(grid: Grid) {
  let [minX, maxX] = [0, grid.length];
  let [minY, maxY] = [0, grid[0].length];
  for (let y = minY; y < maxY; y++) {
    let line = '';
    for (let x = minX; x < maxX; x++) {
      // let value = grid[x][y] !== '.' ? '#' : '.';
      line += grid[x][y];
    }
    console.log(line);
  }
}

function dijkstra(grid: Grid, start: Coord, end: Coord) {
  const queue = new PriorityQueue();
  const distances = new Array(grid.length)
    .fill(null)
    .map(() => new Array(grid[0].length).fill(Infinity));
  distances[start[0]][start[1]] = 0;
  queue.add(start, 0, [start]);
  const visited = new Set();
  const dirs = [
    [-1, 0],
    [0, -1],
    [1, 0],
    [0, 1],
  ];

  while (queue.heap.length > 0) {
    const current = queue.remove();
    const [x, y] = current.val;
    if (visited.has(`${x},${y}`)) continue;
    visited.add(`${x},${y}`);
    if (x === end[0] && y === end[1]) {
      return distances[x][y];
    }
    for (const [dx, dy] of dirs) {
      const newX = x + dx;
      const newY = y + dy;
      if (newX < 0 || newX >= grid.length || newY < 0 || newY >= grid[0].length) {
        continue;
      }
      if (grid[newX][newY] === '#') {
        continue;
      }
      const newDist = distances[x][y] + 1;
      if (newDist < distances[newX][newY]) {
        distances[newX][newY] = newDist;
        queue.add([newX, newY], newDist, [...current.path, [newX, newY]]);
      }
    }
  }
  return -1;
}

export function part1(input: Input) {
  let [maxX, maxY] = [71, 71];
  if (input.length < 100) {
    // Sample input
    maxX = 7;
    maxY = 7;
  }
  let grid = new Array(maxX).fill(null).map(() => new Array(maxY).fill('.'));
  for (let byte of input.slice(0, 1024)) {
    let [x, y] = byte;
    grid[x][y] = '#';
  }
  // printGrid(grid);
  return dijkstra(grid, [0, 0], [maxX - 1, maxY - 1]);
}

export function part2(input: Input) {
  let [maxX, maxY] = [71, 71];
  if (input.length < 100) {
    // Sample input
    maxX = 7;
    maxY = 7;
  }
  let grid = new Array(maxX).fill(null).map(() => new Array(maxY).fill('.'));
  for (let i = 0; i < input.length; i++) {
    let byte = input[i];
    let [x, y] = byte;
    grid[x][y] = '#';
    if (i < 1024) continue;
    let distance = dijkstra(grid, [0, 0], [maxX - 1, maxY - 1]);
    if (distance === -1) return byte.toString();
  }
  return null;
}

function parseInput(inputString: string): Input {
  return inputString
    .trim()
    .split('\n')
    .map(line => line.split(',').map(Number) as Coord);
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
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
