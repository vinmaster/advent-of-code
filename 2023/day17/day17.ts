const DIRS = ['UP', 'RIGHT', 'DOWN', 'LEFT'] as const;
type Direction = (typeof DIRS)[number];
type Coord = [row: number, col: number];
const DIR_DELTA: Record<Direction, Coord> = {
  UP: [-1, 0],
  RIGHT: [0, 1],
  DOWN: [1, 0],
  LEFT: [0, -1],
};

function printGrid(grid) {
  let [minR, maxR] = [0, grid.length];
  let [minC, maxC] = [0, grid[0].length];
  for (let r = minR; r < maxR; r++) {
    let line = '';
    for (let c = minC; c < maxC; c++) {
      let value = grid[r][c] !== '.' ? '#' : '.';
      line += value;
    }
    console.log(line);
  }
}

function opposite(dir: Direction): Direction | null {
  if (dir === null) return null;
  let i = DIRS.indexOf(dir);
  return DIRS[(i + 2) % DIRS.length];
}

function moveCoord(coord: Coord, dir: Direction): Coord {
  let [dr, dc] = DIR_DELTA[dir];
  return [coord[0] + dr, coord[1] + dc];
}

function validCoordGenerator(grid: string[][]) {
  return (coord: Coord): boolean =>
    coord[0] >= 0 && coord[0] < grid.length && coord[1] >= 0 && coord[1] < grid[0].length;
}

function part1(input: string) {
  let grid = input
    .trim()
    .split('\n')
    .map(row => row.split(''));
  let isValid = validCoordGenerator(grid);
  let seen = new Set<string>();
  let queue = new PriorityQueue();
  queue.enqueue({ heatLost: 0, coord: [0, 0], dir: null, straightSteps: 0 }, 0);
  while (queue.values.length > 0) {
    let { heatLost, coord, dir, straightSteps } = queue.dequeue();
    // Reach end goal
    if (coord[0] === grid.length - 1 && coord[1] === grid[0].length - 1) {
      return heatLost;
    }
    if (!isValid(coord)) continue;
    let state = [coord, dir, straightSteps].toString();
    if (seen.has(state)) continue;
    seen.add(state);
    if (straightSteps < 3 && dir !== null) {
      let next = moveCoord(coord, dir);
      if (isValid(next)) {
        let newHeatLost = heatLost + Number(grid[next[0]][next[1]]);
        queue.enqueue(
          { heatLost: newHeatLost, coord: next, dir, straightSteps: straightSteps + 1 },
          -newHeatLost
        );
      }
    }
    for (let d of DIRS) {
      // Not same direction and not opposite
      if (d !== dir && d !== opposite(dir)) {
        let next = moveCoord(coord, d);
        if (isValid(next)) {
          let newHeatLost = heatLost + Number(grid[next[0]][next[1]]);
          queue.enqueue(
            { heatLost: newHeatLost, coord: next, dir: d, straightSteps: 1 },
            -newHeatLost
          );
        }
      }
    }
  }
}

function part2(input: string) {
  let grid = input
    .trim()
    .split('\n')
    .map(row => row.split(''));
  let isValid = validCoordGenerator(grid);
  let seen = new Set<string>();
  let queue = new PriorityQueue();
  queue.enqueue({ heatLost: 0, coord: [0, 0], dir: null, straightSteps: 0 }, 0);
  while (queue.values.length > 0) {
    let { heatLost, coord, dir, straightSteps } = queue.dequeue();
    // Reach end goal
    if (coord[0] === grid.length - 1 && coord[1] === grid[0].length - 1) {
      return heatLost;
    }
    if (!isValid(coord)) continue;
    let state = [coord, dir, straightSteps].toString();
    if (seen.has(state)) continue;
    seen.add(state);
    if (straightSteps < 10 && dir !== null) {
      let next = moveCoord(coord, dir);
      if (isValid(next)) {
        let newHeatLost = heatLost + Number(grid[next[0]][next[1]]);
        queue.enqueue(
          { heatLost: newHeatLost, coord: next, dir, straightSteps: straightSteps + 1 },
          -newHeatLost
        );
      }
    }
    if (straightSteps >= 4 || dir === null) {
      for (let d of DIRS) {
        // Not same direction and not opposite
        if (d !== dir && d !== opposite(dir)) {
          let next = moveCoord(coord, d);
          if (isValid(next)) {
            let newHeatLost = heatLost + Number(grid[next[0]][next[1]]);
            queue.enqueue(
              { heatLost: newHeatLost, coord: next, dir: d, straightSteps: 1 },
              -newHeatLost
            );
          }
        }
      }
    }
  }
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));

/*
// Credit: https://blog.javascripttoday.com/blog/heap-data-structure-in-javascript
class MinHeap {
  constructor() {
    this.heap = [];
  }

  getLeftChildIndex(parentIndex) {
    return 2 * parentIndex + 1;
  }

  getRightChildIndex(parentIndex) {
    return 2 * parentIndex + 2;
  }

  getParentIndex(childIndex) {
    return Math.floor((childIndex - 1) / 2);
  }

  hasParent(index) {
    return this.getParentIndex(index) >= 0;
  }

  swap(index1, index2) {
    [this.heap[index1], this.heap[index2]] = [
      this.heap[index2],
      this.heap[index1],
    ];
  }

  insert(value) {
    this.heap.push(value);
    this.heapifyUp();
  }

  heapifyUp() {
    let currentIndex = this.heap.length - 1;
    while (
      this.hasParent(currentIndex) &&
      this.heap[currentIndex] < this.heap[this.getParentIndex(currentIndex)]
    ) {
      this.swap(currentIndex, this.getParentIndex(currentIndex));
      currentIndex = this.getParentIndex(currentIndex);
    }
  }

  removeMin() {
    if (this.heap.length === 0) {
      throw new Error("Heap is empty");
    }
    const minValue = this.heap[0];
    this.heap[0] = this.heap.pop();
    this.heapifyDown();
    return minValue;
  }

  heapifyDown() {
    let currentIndex = 0;
    while (this.getLeftChildIndex(currentIndex) < this.heap.length) {
      let smallerChildIndex = this.getLeftChildIndex(currentIndex);
      if (
        this.getRightChildIndex(currentIndex) < this.heap.length &&
        this.heap[this.getRightChildIndex(currentIndex)] <
          this.heap[smallerChildIndex]
      ) {
        smallerChildIndex = this.getRightChildIndex(currentIndex);
      }

      if (this.heap[currentIndex] < this.heap[smallerChildIndex]) {
        break;
      } else {
        this.swap(currentIndex, smallerChildIndex);
      }

      currentIndex = smallerChildIndex;
    }
  }
}

const priorityQueue = new MinHeap();
priorityQueue.insert(10);
priorityQueue.insert(5);
priorityQueue.insert(20);
priorityQueue.insert(3);

console.log("Removed min value:", priorityQueue.removeMin());
console.log("Removed min value:", priorityQueue.removeMin()); 
*/

// Credit: https://gist.github.com/ankitamasand/e3758d478451201907f625cc9b1339b3#file-min-heap-js
// class MinHeap {
//   heap: any[];
//   constructor() {
//     /* Initialing the array heap and adding a dummy element at index 0 */
//     this.heap = [null];
//   }
//   getMin() {
//     /* Accessing the min element at index 1 in the heap array */
//     return this.heap[1];
//   }
//   insert(node) {
//     /* Inserting the new node at the end of the heap array */
//     this.heap.push(node);
//     /* Finding the correct position for the new node */
//     if (this.heap.length > 1) {
//       let current = this.heap.length - 1;
//       /* Traversing up the parent node until the current node (current) is greater than the parent (current/2)*/
//       while (current > 1 && this.heap[Math.floor(current / 2)] > this.heap[current]) {
//         /* Swapping the two nodes by using the ES6 destructuring syntax*/
//         [this.heap[Math.floor(current / 2)], this.heap[current]] = [
//           this.heap[current],
//           this.heap[Math.floor(current / 2)],
//         ];
//         current = Math.floor(current / 2);
//       }
//     }
//   }
//   remove() {
//     /* Smallest element is at the index 1 in the heap array */
//     let smallest = this.heap[1];
//     /* When there are more than two elements in the array, we put the right most element at the first position
//           and start comparing nodes with the child nodes
//       */
//     if (this.heap.length > 2) {
//       this.heap[1] = this.heap[this.heap.length - 1];
//       this.heap.splice(this.heap.length - 1);
//       if (this.heap.length === 3) {
//         if (this.heap[1] > this.heap[2]) {
//           [this.heap[1], this.heap[2]] = [this.heap[2], this.heap[1]];
//         }
//         return smallest;
//       }
//       let current = 1;
//       let leftChildIndex = current * 2;
//       let rightChildIndex = current * 2 + 1;
//       while (
//         this.heap[leftChildIndex] &&
//         this.heap[rightChildIndex] &&
//         (this.heap[current] > this.heap[leftChildIndex] ||
//           this.heap[current] > this.heap[rightChildIndex])
//       ) {
//         if (this.heap[leftChildIndex] < this.heap[rightChildIndex]) {
//           [this.heap[current], this.heap[leftChildIndex]] = [
//             this.heap[leftChildIndex],
//             this.heap[current],
//           ];
//           current = leftChildIndex;
//         } else {
//           [this.heap[current], this.heap[rightChildIndex]] = [
//             this.heap[rightChildIndex],
//             this.heap[current],
//           ];
//           current = rightChildIndex;
//         }
//         leftChildIndex = current * 2;
//         rightChildIndex = current * 2 + 1;
//       }
//     } else if (this.heap.length === 2) {
//       /* If there are only two elements in the array, we directly splice out the first element */
//       this.heap.splice(1, 1);
//     } else {
//       return null;
//     }
//     return smallest;
//   }
// }

// Credit: https://stackoverflow.com/questions/35978242/min-heap-in-javascript
class MinHeap {
  heap: any;
  constructor(array) {
    this.heap = this.buildHeap(array);
  }
  // O(n) time | O(1) space
  buildHeap(array) {
    const firstParentIdx = Math.floor((array.length - 2) / 2);
    for (let currentIdx = firstParentIdx; currentIdx >= 0; currentIdx--) {
      this.siftDown(currentIdx, array.length - 1, array);
    }
    return array;
  }
  // O(log(n)) time | O(1) space
  siftDown(currentIdx, endIdx, heap) {
    let childOneIdx = currentIdx * 2 + 1;
    while (childOneIdx <= endIdx) {
      const childTwoIdx = currentIdx * 2 + 2 <= endIdx ? currentIdx * 2 + 2 : -1;
      let idxToSwap;
      if (childTwoIdx !== -1 && heap[childTwoIdx] < heap[childOneIdx]) {
        idxToSwap = childTwoIdx;
      } else {
        idxToSwap = childOneIdx;
      }
      if (heap[idxToSwap] < heap[currentIdx]) {
        this.swap(currentIdx, idxToSwap, heap);
        currentIdx = idxToSwap;
        childOneIdx = currentIdx * 2 + 1;
      } else {
        return;
      }
    }
  }
  // O(log(n)) time | O(1) space
  siftUp(currentIdx, heap) {
    let parentIdx = Math.floor((currentIdx - 1) / 2);
    while (currentIdx > 0 && heap[currentIdx] < heap[parentIdx]) {
      this.swap(currentIdx, parentIdx, heap);
      currentIdx = parentIdx;
      parentIdx = Math.floor((currentIdx - 1) / 2);
    }
  }
  // O(1)  time | O(1) space
  peek() {
    return this.heap[0];
  }
  // O(log(n)) time | O(1) space
  remove() {
    this.swap(0, this.heap.length - 1, this.heap);
    const valueToRemove = this.heap.pop();
    this.siftDown(0, this.heap.length - 1, this.heap);
    return valueToRemove;
  }
  // O(log(n)) time | O(1) space
  insert(value) {
    this.heap.push(value);
    this.siftUp(this.heap.length - 1, this.heap);
  }
  swap(i, j, heap) {
    [heap[i], heap[j]] = [heap[j], heap[i]];
  }
}

// let heap = new MinHeap([31, 62, 25, 42, 35]);
// console.log(heap.peek());

class BinaryHeap {
  values: any[];
  constructor() {
    this.values = [];
  }
  add(element) {
    this.values.push(element);
    let index = this.values.length - 1;
    const current = this.values[index];
    while (index > 0) {
      let parentIndex = Math.floor((index - 1) / 2);
      let parent = this.values[parentIndex];
      if (parent <= current) {
        this.values[parentIndex] = current;
        this.values[index] = parent;
        index = parentIndex;
      } else break;
    }
  }
  extractMax() {
    const max = this.values[0];
    const end = this.values.pop();
    this.values[0] = end;
    let index = 0;
    const length = this.values.length;
    const current = this.values[0];
    while (true) {
      let leftChildIndex = 2 * index + 1;
      let rightChildIndex = 2 * index + 2;
      let leftChild, rightChild;
      let swap: any = null;
      if (leftChildIndex < length) {
        leftChild = this.values[leftChildIndex];
        if (leftChild > current) swap = leftChildIndex;
      }
      if (rightChildIndex < length) {
        rightChild = this.values[rightChildIndex];
        if ((swap === null && rightChild > current) || (swap !== null && rightChild > leftChild))
          swap = rightChildIndex;
      }
      if (swap === null) break;
      this.values[index] = this.values[swap];
      this.values[swap] = current;
      index = swap;
    }
    return max;
  }
}

// const tree = new BinaryHeap();
// tree.add(3);
// tree.add(4);
// tree.add(31);
// tree.add(6);
// console.log(tree.values); // [31, 6, 4, 3]
// console.log(tree.extractMax()); // 31

class HeapNode {
  val: any;
  priority: number;
  constructor(val: any, priority: number) {
    this.val = val;
    this.priority = priority;
  }
}
class PriorityQueue {
  values: any[];
  constructor() {
    this.values = [];
  }
  enqueue(val, priority: number) {
    let newNode = new HeapNode(val, priority);
    this.values.push(newNode);
    let index = this.values.length - 1;
    const current = this.values[index];
    while (index > 0) {
      let parentIndex = Math.floor((index - 1) / 2);
      let parent = this.values[parentIndex];
      if (parent.priority <= current.priority) {
        this.values[parentIndex] = current;
        this.values[index] = parent;
        index = parentIndex;
      } else break;
    }
  }
  dequeue() {
    const max = this.values[0];
    const end = this.values.pop();
    this.values[0] = end;
    let index = 0;
    const length = this.values.length;
    const current = this.values[0];
    while (true) {
      let leftChildIndex = 2 * index + 1;
      let rightChildIndex = 2 * index + 2;
      let leftChild, rightChild;
      let swap: any = null;
      if (leftChildIndex < length) {
        leftChild = this.values[leftChildIndex];
        if (leftChild.priority > current.priority) swap = leftChildIndex;
      }
      if (rightChildIndex < length) {
        rightChild = this.values[rightChildIndex];
        if (
          (swap === null && rightChild.priority > current.priority) ||
          (swap !== null && rightChild.priority > leftChild.priority)
        )
          swap = rightChildIndex;
      }
      if (swap === null) break;
      this.values[index] = this.values[swap];
      this.values[swap] = current;
      index = swap;
    }
    return max.val;
  }
}

// const tree = new PriorityQueue();
// tree.enqueue(3, 2);
// tree.enqueue(4, 5);
// tree.enqueue(31, 1);
// tree.enqueue(6, 3);
// console.log(tree.dequeue()); // 4
// console.log(tree.dequeue()); // 6
// console.log(tree.dequeue()); // 3
// console.log(tree.dequeue()); // 31

/*
Credit:
https://www.redblobgames.com/pathfinding/a-star/implementation.html
https://hankus.github.io/javascript-astar/docs/astar.html
*/
// breadth first search
function bfs(graph, start: string) {
  let getNeighbors = (graph, node) => graph[node];
  let queue: string[] = [start];
  let visited = new Set<string>();
  visited.add(start);
  while (queue.length > 0) {
    let current = queue.shift();
    console.log('visiting', current);
    for (let next of getNeighbors(graph, current)) {
      if (!visited.has(next)) {
        queue.push(next);
        visited.add(next);
      }
    }
  }
}

// const graph = {
//   A: ['B'],
//   B: ['A', 'C', 'D'],
//   C: ['A'],
//   D: ['E', 'A'],
//   E: ['B'],
// };
// console.log('bfs', bfs(graph, 'A'));

function dijkstra(graph, start: string, end: string) {
  let getNeighbors = (graph, node) => graph[node];
  let getCost = (a, b) => 1;
  let queue = new PriorityQueue();
  queue.enqueue(start, 0);
  let cameFrom: Record<string, string> = {};
  let costs: Record<string, number> = {};
  cameFrom[start] = start;
  costs[start] = 0;
  while (queue.values.length > 0) {
    let current = queue.dequeue();
    if (current === end) {
      return costs[current];
    }
    for (let next of getNeighbors(graph, current)) {
      let newCost = costs[current] + getCost(current, next);
      if (costs[next] === undefined || newCost < costs[next]) {
        costs[next] = newCost;
        let priority = newCost;
        queue.enqueue(next, priority);
        cameFrom[next] = current;
      }
    }
  }
}

// const graph = {
//   A: ['B'],
//   B: ['A', 'C', 'D'],
//   C: ['A'],
//   D: ['E', 'A'],
//   E: ['B'],
// };
// console.log('dijkstra distance', dijkstra(graph, 'A', 'E'));

function astar(graph, start: string, end: string) {
  let getNeighbors = (graph, node) => graph[node];
  let getCost = (a, b) => 1;
  let heuristic = (a, b) => 1;
  let queue = new PriorityQueue();
  queue.enqueue(start, 0);
  let cameFrom: Record<string, string> = {};
  let costs: Record<string, number> = {};
  cameFrom[start] = start;
  costs[start] = 0;
  while (queue.values.length > 0) {
    let current = queue.dequeue();
    if (current === end) {
      return costs[current];
    }
    for (let next of getNeighbors(graph, current)) {
      let newCost = costs[current] + getCost(current, next);
      if (costs[next] === undefined || newCost < costs[next]) {
        costs[next] = newCost;
        let priority = newCost + heuristic(next, end);
        queue.enqueue(next, priority);
        cameFrom[next] = current;
      }
    }
  }
}

// const graph = {
//   A: ['B'],
//   B: ['A', 'C', 'D'],
//   C: ['A'],
//   D: ['E', 'A'],
//   E: ['B'],
// };
// console.log('a* distance', astar(graph, 'A', 'E'));
