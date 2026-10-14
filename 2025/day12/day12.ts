type Point = { x: number; y: number };
type Shape = Point[];

export type Input = {
  shapes: Shape[][]; // Array of shapes, where each shape is an array of its unique orientations
  regions: { size: [number, number]; quantities: number[] }[];
};

// Normalizes a shape by shifting it so its top-leftmost block is at (0,0)
function normalize(shape: Shape): Shape {
  const minX = Math.min(...shape.map(p => p.x));
  const minY = Math.min(...shape.map(p => p.y));
  return shape
    .map(p => ({ x: p.x - minX, y: p.y - minY }))
    .sort((a, b) => (a.y === b.y ? a.x - b.x : a.y - b.y));
}

// Generates all unique orientations (rotations and flips) of a shape
function getUniqueOrientations(shape: Shape): Shape[] {
  const orientations = new Set<string>();
  const result: Shape[] = [];

  for (const flip of [false, true]) {
    let current = shape;
    if (flip) {
      current = current.map(p => ({ x: -p.x, y: p.y }));
    }
    // 4 rotations
    for (let rot = 0; rot < 4; rot++) {
      current = current.map(p => ({ x: -p.y, y: p.x }));
      const norm = normalize(current);
      const str = JSON.stringify(norm);
      if (!orientations.has(str)) {
        orientations.add(str);
        result.push(norm);
      }
    }
  }
  return result;
}

// Optimized Backtracking DFS using BigInt bitboards
function canFit(
  width: number,
  height: number,
  quantities: number[],
  allShapes: Shape[][]
): boolean {
  // Pre-check 1: Do we have enough total cells to fit the requested volume?
  let totalBlocks = 0;
  for (let i = 0; i < quantities.length; i++) {
    if (quantities[i] > 0) {
      totalBlocks += quantities[i] * allShapes[i][0].length;
    }
  }
  if (totalBlocks > width * height) return false;

  type Task = { id: number; count: number; masks: bigint[]; size: number };
  const tasks: Task[] = [];

  // Pre-calculate all valid valid placement masks for the pieces
  for (let i = 0; i < quantities.length; i++) {
    if (quantities[i] > 0) {
      const masks: bigint[] = [];
      const orientations = allShapes[i];

      for (const ori of orientations) {
        const maxX = Math.max(...ori.map(p => p.x));
        const maxY = Math.max(...ori.map(p => p.y));

        // Slide the orientation across all valid grid positions
        for (let y = 0; y <= height - 1 - maxY; y++) {
          for (let x = 0; x <= width - 1 - maxX; x++) {
            let mask = 0n;
            for (const p of ori) {
              // Map the (x, y) coordinate to a single bit position
              const bitPos = BigInt((y + p.y) * width + (x + p.x));
              mask |= 1n << bitPos;
            }
            masks.push(mask);
          }
        }
      }

      // Pre-check 2: If a piece has fewer valid positions than we need copies of it, it's impossible
      if (masks.length < quantities[i]) return false;

      tasks.push({
        id: i,
        count: quantities[i],
        masks: masks,
        size: orientations[0].length,
      });
    }
  }

  // Optimization: Place the most constrained (largest) shapes first
  tasks.sort((a, b) => b.size - a.size);

  function solve(
    taskIdx: number,
    qtyRemaining: number,
    maskStartIdx: number,
    currentGrid: bigint
  ): boolean {
    // If we've successfully processed all tasks, everything fits
    if (taskIdx === tasks.length) return true;

    const task = tasks[taskIdx];

    // If we finished placing all pieces of the current shape type, move to the next
    if (qtyRemaining === 0) {
      return solve(taskIdx + 1, tasks[taskIdx + 1]?.count || 0, 0, currentGrid);
    }

    const masks = task.masks;

    // Search combinations to prevent identical piece permutation overhead.
    // By enforcing `maskStartIdx`, identical pieces are always chosen in strictly increasing
    // order from our mask pool, entirely eliminating symmetry processing.
    for (let i = maskStartIdx; i < masks.length; i++) {
      const m = masks[i];

      // Fast overlap check using bitwise AND
      if ((currentGrid & m) === 0n) {
        // Place piece using bitwise OR, recurse for the next identical piece
        if (solve(taskIdx, qtyRemaining - 1, i + 1, currentGrid | m)) {
          return true;
        }
      }
    }

    return false;
  }

  // Edge case: region requires no presents
  if (tasks.length === 0) return true;

  return solve(0, tasks[0].count, 0, 0n);
}

export function part1(input: Input) {
  let count = 0;

  for (let i = 0; i < input.regions.length; i++) {
    const region = input.regions[i];
    const [w, h] = region.size;

    if (canFit(w, h, region.quantities, input.shapes)) {
      count++;
    }
  }

  console.log('part1:', count);
  return count;
}

function parseInput(inputString: string): Input {
  // Split blocks by double (or more) newlines
  const sections = inputString.trim().split(/\n\s*\n/);

  const section2 = sections.pop()!;
  const shapes: Shape[][] = [];

  for (const section of sections) {
    const lines = section.trim().split('\n');
    const idLine = lines.shift()!;
    const id = parseInt(idLine.replace(':', ''), 10);

    const shapePoints: Shape = [];
    for (let y = 0; y < lines.length; y++) {
      for (let x = 0; x < lines[y].length; x++) {
        if (lines[y][x] === '#') {
          shapePoints.push({ x, y });
        }
      }
    }
    shapes[id] = getUniqueOrientations(shapePoints);
  }

  const regions: { size: [number, number]; quantities: number[] }[] = [];

  for (const line of section2.trim().split('\n')) {
    const [sizePart, qtyPart] = line.split(': ');
    const [width, height] = sizePart.split('x').map(Number);
    const quantities = qtyPart.trim().split(/\s+/).map(Number);

    regions.push({ size: [width, height], quantities });
  }

  return { shapes, regions };
}

async function main(useRealInput = true) {
  let inputString = '';
  try {
    // @ts-expect-error: next-line
    inputString = await Bun.file(`${import.meta.dir}/input.txt`).text();
  } catch (error) {
    useRealInput = false;
  }

  const testInput = `
0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2`;

  if (!useRealInput) inputString = testInput;

  part1(parseInput(inputString));
}

// await main(false);
await main();
