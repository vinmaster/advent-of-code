// deno-lint-ignore-file prefer-const no-explicit-any

// credit: https://dev.to/nickymeuleman/advent-of-code-2022-day-24-44dg

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// #.######
// #>>.<^<#
// #.<..<<#
// #>v.><>#
// #<^v^^>#
// ######.#`;

function part1(input: string) {
  input = input.trim();
  let grid = parseGrid(input);
  let start = [0, grid[0].findIndex(x => x === '.')] as Coord;
  let end = [grid.length - 1, grid.at(-1)?.findIndex(x => x === '.')] as Coord;
  let walls = new Set<CoordStr>(
    grid
      .flatMap((row, r) =>
        row.map((tile, c) => {
          if (tile === '#') return toStr([r, c]);
          return null;
        })
      )
      .filter(x => x !== null) as CoordStr[]
  );
  // cover up the start/end
  walls.add(toStr([start[0] - 1, start[1]]));
  walls.add(toStr([end[0] + 1, end[1]]));
  let lcmNum = lcm(grid.length - 2, grid[0].length - 2);
  let blizzardMaps = getBlizzardMaps(grid, lcmNum);
  let mapInfo = {
    grid,
    walls,
    blizzardMaps,
    repeatsAt: lcmNum,
  };
  return shortest(start, end, 0, mapInfo);
}

function part2(input: string) {
  input = input.trim();
  let grid = parseGrid(input);
  let start = [0, grid[0].findIndex(x => x === '.')] as Coord;
  let end = [grid.length - 1, grid.at(-1)?.findIndex(x => x === '.')] as Coord;
  let walls = new Set<CoordStr>(
    grid
      .flatMap((row, r) =>
        row.map((tile, c) => {
          if (tile === '#') return toStr([r, c]);
          return null;
        })
      )
      .filter(x => x !== null) as CoordStr[]
  );
  // cover up the start/end
  walls.add(toStr([start[0] - 1, start[1]]));
  walls.add(toStr([end[0] + 1, end[1]]));
  let lcmNum = lcm(grid.length - 2, grid[0].length - 2);
  let blizzardMaps = getBlizzardMaps(grid, lcmNum);
  let mapInfo = {
    grid,
    walls,
    blizzardMaps,
    repeatsAt: lcmNum,
  };
  let trip1Time = shortest(start, end, 0, mapInfo);
  let trip2Time = shortest(end, start, trip1Time, mapInfo);
  return shortest(start, end, trip2Time, mapInfo);
}

const DIRS: Record<string, Coord> = {
  '^': [-1, 0],
  '>': [0, 1],
  v: [1, 0],
  '<': [0, -1],
};

type Coord = [row: number, col: number];
type CoordStr = `${number},${number}`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));

function parseGrid(map: string): string[][] {
  return map.split('\n').map(line => line.split(''));
}

function toStr(coord: Coord): CoordStr {
  return coord.toString() as CoordStr;
}

function toCoord(str: CoordStr): Coord {
  return str.split(',').map(Number) as Coord;
}

function toMap(grid: string[][]): Record<CoordStr, string> {
  let map = {} as Record<CoordStr, string>;
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[row].length; col++) {
      map[`${row},${col}`] = grid[row][col];
    }
  }
  return map;
}

function addCoord(c1: Coord, c2: Coord): Coord {
  return [c1[0] + c2[0], c1[1] + c2[1]];
}

function inBound(grid: string[][], c: Coord): boolean {
  return c[0] >= 0 && c[0] < grid.length && c[1] >= 0 && c[1] < grid[0].length;
}

function print(grid: string[][]) {
  for (let row of grid) {
    console.log(row.join(''));
  }
}

// least common multiple
function lcm(a: number, b: number): number {
  return (a * b) / gcd(a, b);
}

// greatest common divisor
function gcd(a: number, b: number): number {
  let max = a;
  let min = b;
  if (min > max) {
    [min, max] = [max, min];
  }
  while (true) {
    let res = max % min;
    if (res === 0) return min;
    max = min;
    min = res;
  }
}

function getBlizzardMaps(grid: string[][], maxTime: number) {
  let cache = {} as Record<number, CoordStr[]>;
  let blizzards = Object.entries(toMap(grid))
    .map(([coordStr, tile]) => {
      if (tile === '#' || tile === '.') return null;
      return [toCoord(coordStr as any), tile];
    })
    .filter(x => x !== null) as [Coord, string][];
  cache[0] = blizzards.map(([coord, _]) => toStr(coord));
  // cache every blizzard coord at every time
  for (let time = 1; time < maxTime; time++) {
    for (let [coord, tile] of blizzards) {
      let newCoord = move(grid, coord, tile);
      [coord[0], coord[1]] = [newCoord[0], newCoord[1]];
    }
    cache[time] = blizzards.map(([coord, _]) => toStr(coord));
  }
  return cache;
}

function move(grid: string[][], coord: Coord, dir: string): Coord {
  let newCoord = addCoord(coord, DIRS[dir]);
  // wrapping if next coord is edge
  if (dir === '<' && newCoord[1] === 0) {
    newCoord[1] = grid[0].length - 2;
  } else if (dir === '>' && newCoord[1] === grid[0].length - 1) {
    newCoord[1] = 1;
  } else if (dir === '^' && newCoord[0] === 0) {
    newCoord[0] = grid.length - 2;
  } else if (dir === 'v' && newCoord[0] === grid.length - 1) {
    newCoord[0] = 1;
  }
  return newCoord;
}

function neighbourCoords(grid: string[][], coord: Coord) {
  return Object.keys(DIRS)
    .map(d => move(grid, coord, d))
    .filter(c => inBound(grid, c));
}

function mod(n: number, d: number): number {
  return ((n % d) + d) % d;
}

function shortest(
  from: Coord,
  to: Coord,
  startTime: number,
  mapInfo: {
    grid: string[][];
    walls: Set<CoordStr>;
    blizzardMaps: Record<number, CoordStr[]>;
    repeatsAt: number;
  }
) {
  let { grid, walls, blizzardMaps, repeatsAt } = mapInfo;

  let time = startTime;
  let queue = new Set<CoordStr>();
  queue.add(toStr(from));

  while (queue.size > 0) {
    let nextQueue = new Set<CoordStr>();
    // blizzards = blizzards.map(b => {
    //   let coord = move(grid, b.coord, b.dir);
    //   return { coord, dir: b.dir };
    // });
    let blizzards = blizzardMaps[time % repeatsAt];
    let blizzardCoordStrs = new Set<CoordStr>(blizzards.map(b => b));
    for (let coordStr of queue) {
      if (coordStr === toStr(to)) return time - 1;
      if (!blizzardCoordStrs.has(coordStr)) nextQueue.add(coordStr);
      let candidates = Object.values(DIRS)
        .map(d => addCoord(d, toCoord(coordStr)))
        .map(toStr)
        .filter(s => !blizzardCoordStrs.has(s) && !walls.has(s));
      for (let c of candidates) {
        nextQueue.add(c);
      }
    }
    queue = nextQueue;
    time += 1;
  }
  return time;
}

function part1Attemp3(input: string) {
  input = input.trim();
  let grid = parseGrid(input);
  let start = [0, grid[0].findIndex(x => x === '.')] as Coord;
  let end = [grid.length - 1, grid.at(-1)?.findIndex(x => x === '.')] as Coord;
  let blizzards = input
    .split(/\r?\n/)
    .slice(1, -1)
    .map(line => line.substring(1, line.length - 1));
  let getKey = (coord: Coord, time: number) =>
    `${toStr(coord)}-${time % ((grid.length - 2) * (grid[0].length - 2))}`;
  let canMove = (grid: string[][], coord: Coord, time: number, visited: Set<string>) => {
    if (toStr(coord) === toStr(start) || toStr(coord) === toStr(end)) return true;
    if (
      coord[0] < 0 ||
      coord[0] > grid.length - 2 ||
      coord[0] === 0 ||
      coord[0] === grid.length - 1 ||
      coord[1] === 0 ||
      coord[1] === grid[0].length - 1
    )
      return false;
    return !hasBlizzard(grid, blizzards, coord, time) && !visited.has(getKey(coord, time));
  };
  let visited = new Set<string>();
  let stack = [{ coord: start, time: 0 }] as { coord: Coord; time: number }[];
  while (stack.length > 0) {
    let { coord, time } = stack.shift()!;
    if (toStr(coord) === toStr(end)) return time;
    let next = [] as { coord: Coord; time: number }[];
    let candidates = Object.values(DIRS).map(d => addCoord(d, coord));
    if (canMove(grid, coord, time + 1, visited)) next.push({ coord, time: time + 1 });
    for (let c of candidates) {
      if (canMove(grid, c, time + 1, visited)) {
        next.push({ coord: c, time: time + 1 });
      }
    }
    for (let nextState of next) {
      stack.push(nextState);
      visited.add(getKey(nextState.coord, nextState.time));
    }
  }
  return -1;
}

function hasBlizzard(grid: string[][], blizzards: string[], coord: Coord, time: number): boolean {
  let [row, col] = coord;
  row -= 1;
  col -= 1;
  return (
    blizzards[mod(row - time, grid.length - 2)][col] === 'v' ||
    blizzards[mod(row + time, grid.length - 2)][col] === '^' ||
    blizzards[row][mod(col - time, grid[0].length - 2)] === '>' ||
    blizzards[row][mod(col + time, grid[0].length - 2)] === '<'
  );
}

function part1Attemp2(input: string) {
  input = input.trim();
  let grid = parseGrid(input);
  let start = [0, grid[0].findIndex(x => x === '.')] as Coord;
  let end = [grid.length - 1, grid.at(-1)?.findIndex(x => x === '.')] as Coord;
  let walls = new Set<CoordStr>(
    grid
      .flatMap((row, r) =>
        row.map((tile, c) => {
          if (tile === '#') return toStr([r, c]);
          return null;
        })
      )
      .filter(x => x !== null) as CoordStr[]
  );
  // cover up the start/end
  walls.add(toStr([start[0] - 1, start[1]]));
  walls.add(toStr([end[0] + 1, end[1]]));
  let blizzards = grid
    .flatMap((row, r) =>
      row.map((tile, c) => {
        if (tile !== '#' && tile !== '.' && tile.trim() !== '') {
          return { coord: [r, c], dir: tile };
        }
        return null;
      })
    )
    .filter(x => x !== null) as { coord: Coord; dir: string }[];
  let time = 0;
  let queue = new Set<CoordStr>();
  queue.add(toStr(start));

  while (queue.size > 0) {
    let nextQueue = new Set<CoordStr>();
    blizzards = blizzards.map(b => {
      let coord = move(grid, b.coord, b.dir);
      return { coord, dir: b.dir };
    });
    let blizzardCoordStrs = new Set<CoordStr>(blizzards.map(b => toStr(b.coord)));
    for (let coordStr of queue) {
      if (coordStr === toStr(end)) return time;
      if (!blizzardCoordStrs.has(coordStr)) nextQueue.add(coordStr);
      let candidates = Object.values(DIRS)
        .map(d => addCoord(d, toCoord(coordStr)))
        .map(toStr)
        .filter(s => !blizzardCoordStrs.has(s) && !walls.has(s));
      for (let c of candidates) {
        nextQueue.add(c);
      }
    }
    queue = nextQueue;
    time += 1;
  }
  return time;
}

function part1Attemp1(input: string) {
  input = input.trim();
  let grid = parseGrid(input);
  let [rows, cols] = [grid.length, grid[0].length];
  let walls = Object.entries(toMap(grid))
    .map(([coordStr, tile]) => {
      if (tile !== '#') return null;
      return coordStr;
    })
    .filter(x => x !== null) as CoordStr[];
  let lcmNum = lcm(rows - 2, cols - 2);
  let blizzardMaps = getBlizzardMaps(grid, lcmNum);
  let start = [0, 1] as Coord;
  let end = [rows - 1, cols - 2] as Coord;

  let queue = [] as { cost: number; coord: Coord }[];
  let seen = new Set();

  queue.push({ cost: 0, coord: start });
  seen.add(JSON.stringify([start, 0]));

  while (queue.length > 0) {
    let { cost, coord } = queue.shift()!;
    if (toStr(coord) === toStr(end)) {
      return cost;
    }
    let newCost = cost + 1;
    let blizzards = blizzardMaps[newCost % lcmNum];

    let candidates = [coord, ...neighbourCoords(grid, coord)]
      .map(toStr)
      .filter(c => !walls.includes(c))
      .filter(c => !blizzards.includes(c))
      .map(toCoord);
    for (let newCoord of candidates) {
      let key = JSON.stringify([newCoord, newCost]);
      if (!seen.has(key)) {
        seen.add(key);
        queue.push({ cost: newCost, coord: newCoord });
      }
    }
  }
  // print(grid);
}

// -------------------------------------------------------

// const bfs = (rows, states, starting, ending, startingTime) => {
//   let queue = [{ position: { x: starting.x, y: starting.y }, minute: startingTime }];
//   let visited = [`${starting.x},${starting.y},0`];
//   while (queue.length > 0) {
//     let current = queue.shift();

//     let nextMove = [
//       { x: current.position.x + 1, y: current.position.y },
//       { x: current.position.x, y: current.position.y + 1 },
//       { x: current.position.x - 1, y: current.position.y },
//       { x: current.position.x, y: current.position.y - 1 },
//       { x: current.position.x, y: current.position.y },
//     ];

//     for (let move of nextMove) {
//       if (move.x == ending.x && move.y == ending.y) return current.minute + 1;

//       if (
//         move.x < 0 ||
//         move.x >= rows[0].length ||
//         move.y < 0 ||
//         move.y >= rows.length ||
//         states[(current.minute + 1) % (rows.length * rows[0].length)].has(`${move.x},${move.y}`) ||
//         visited.includes(`${move.x},${move.y},${current.minute + 1}`) ||
//         rows[move.y][move.x] == '#'
//       ) {
//         continue;
//       }

//       visited.push(`${move.x},${move.y},${current.minute + 1}`);
//       queue.push({ position: move, minute: current.minute + 1 });
//     }
//   }
// };
