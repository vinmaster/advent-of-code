// Define the problem's constants
const ENERGY_COST: { [key: string]: number } = {
  A: 1,
  B: 10,
  C: 100,
  D: 1000,
};

const TARGET_ROOM: { [key: string]: number } = {
  A: 0,
  B: 1,
  C: 2,
  D: 3,
};

// The hallway index directly above each room
const ROOM_HALLWAY_POS = [2, 4, 6, 8];

// The 7 valid spots in the hallway where an amphipod can stop
const VALID_HALLWAY_SPOTS = [0, 1, 3, 5, 7, 9, 10];

class PriorityQueue<T> {
  private heap: { cost: number; item: T }[] = [];

  push(item: T, cost: number) {
    this.heap.push({ item, cost });
    this.bubbleUp(this.heap.length - 1);
  }

  pop(): { item: T; cost: number } | undefined {
    if (this.isEmpty()) return undefined;
    const min = this.heap[0];
    const end = this.heap.pop()!;
    if (this.heap.length > 0) {
      this.heap[0] = end;
      this.bubbleDown(0);
    }
    return min;
  }

  isEmpty(): boolean {
    return this.heap.length === 0;
  }

  private bubbleUp(index: number) {
    while (index > 0) {
      const parentIndex = Math.floor((index - 1) / 2);
      if (this.heap[index].cost < this.heap[parentIndex].cost) {
        [this.heap[index], this.heap[parentIndex]] = [this.heap[parentIndex], this.heap[index]];
        index = parentIndex;
      } else {
        break;
      }
    }
  }

  private bubbleDown(index: number) {
    const lastIndex = this.heap.length - 1;
    while (true) {
      let leftChildIdx = 2 * index + 1;
      let rightChildIdx = 2 * index + 2;
      let smallestIdx = index;

      if (leftChildIdx <= lastIndex && this.heap[leftChildIdx].cost < this.heap[smallestIdx].cost) {
        smallestIdx = leftChildIdx;
      }

      if (
        rightChildIdx <= lastIndex &&
        this.heap[rightChildIdx].cost < this.heap[smallestIdx].cost
      ) {
        smallestIdx = rightChildIdx;
      }

      if (smallestIdx !== index) {
        [this.heap[index], this.heap[smallestIdx]] = [this.heap[smallestIdx], this.heap[index]];
        index = smallestIdx;
      } else {
        break;
      }
    }
  }
}

type State = string;

function parseState(stateStr: State, roomDepth: number) {
  const hallway = stateStr.substring(0, 11).split('');
  const roomStr = stateStr.substring(12);
  const rooms: string[][] = [[], [], [], []];
  for (let r = 0; r < 4; r++) {
    for (let d = 0; d < roomDepth; d++) {
      rooms[r].push(roomStr[r * roomDepth + d]);
    }
  }
  return { hallway, rooms };
}

function serializeState(hallway: string[], rooms: string[][], roomDepth: number): State {
  const hallStr = hallway.join('');
  let roomStr = '';
  for (let r = 0; r < 4; r++) {
    for (let d = 0; d < roomDepth; d++) {
      roomStr += rooms[r][d];
    }
  }
  return `${hallStr}_${roomStr}`;
}

function getValidMoves(
  stateStr: State,
  roomDepth: number
): { nextState: State; moveCost: number }[] {
  const moves: { nextState: State; moveCost: number }[] = [];
  const { hallway, rooms } = parseState(stateStr, roomDepth);

  for (let r = 0; r < 4; r++) {
    let spot = -1; // The index (depth) of the first amphipod found
    let amphipod = '.';

    // Find the topmost amphipod in this room
    for (let d = 0; d < roomDepth; d++) {
      if (rooms[r][d] !== '.') {
        spot = d;
        amphipod = rooms[r][d];
        break;
      }
    }

    if (amphipod === '.') continue; // Room is empty

    const targetRoom = TARGET_ROOM[amphipod];

    // Check if it *should* move.
    // It should not move if it's already in its correct room
    // AND all amphipods below it are also correct.
    let isHomeAndHappy = r === targetRoom;
    if (isHomeAndHappy) {
      for (let d = spot + 1; d < roomDepth; d++) {
        if (TARGET_ROOM[rooms[r][d]] !== r) {
          isHomeAndHappy = false; // Blocking a wrong one
          break;
        }
      }
    }
    if (isHomeAndHappy) continue; // Don't move

    const stepsToHall = spot + 1;
    const startHallPos = ROOM_HALLWAY_POS[r];

    // Check all 7 valid hallway destinations
    for (const h of VALID_HALLWAY_SPOTS) {
      // Check if the path in the hallway is clear
      let pathClear = true;
      const dir = startHallPos < h ? 1 : -1;

      for (let i = startHallPos + dir; i !== h + dir; i += dir) {
        if (hallway[i] !== '.') {
          pathClear = false;
          break;
        }
      }

      if (pathClear) {
        const stepsToDest = Math.abs(h - startHallPos);
        const totalSteps = stepsToHall + stepsToDest;
        const moveCost = totalSteps * ENERGY_COST[amphipod];

        // Create the new state
        const newHallway = [...hallway];
        const newRooms = rooms.map(room => [...room]);
        newHallway[h] = amphipod;
        newRooms[r][spot] = '.';

        moves.push({
          nextState: serializeState(newHallway, newRooms, roomDepth),
          moveCost,
        });
      }
    }
  }

  for (let h = 0; h < hallway.length; h++) {
    const amphipod = hallway[h];
    if (amphipod === '.') continue;

    const targetRoom = TARGET_ROOM[amphipod];
    const destHallPos = ROOM_HALLWAY_POS[targetRoom];
    const room = rooms[targetRoom];

    // Check room condition: Can only enter if empty or contains only correct type
    if (room.some(a => a !== '.' && TARGET_ROOM[a] !== targetRoom)) {
      continue;
    }

    // Check if hallway path is clear
    let pathClear = true;
    const dir = h < destHallPos ? 1 : -1;
    for (let i = h + dir; i !== destHallPos + dir; i += dir) {
      if (hallway[i] !== '.') {
        pathClear = false;
        break;
      }
    }

    if (pathClear) {
      // Find the deepest available spot in the room
      let destSpot = -1;
      for (let d = roomDepth - 1; d >= 0; d--) {
        if (room[d] === '.') {
          destSpot = d;
          break;
        }
      }

      // This should never happen if the room condition check passed,
      // but as a safeguard, we skip if the room is full.
      if (destSpot === -1) continue;

      const stepsToRoomDoor = Math.abs(h - destHallPos);
      const stepsIntoRoom = destSpot + 1;
      const totalSteps = stepsToRoomDoor + stepsIntoRoom;
      const moveCost = totalSteps * ENERGY_COST[amphipod];

      // Create the new state
      const newHallway = [...hallway];
      const newRooms = rooms.map(room => [...room]);
      newHallway[h] = '.';
      newRooms[targetRoom][destSpot] = amphipod;

      moves.push({
        nextState: serializeState(newHallway, newRooms, roomDepth),
        moveCost,
      });
    }
  }

  return moves;
}

function solve(initialState: State, targetState: State, roomDepth: number) {
  const pq = new PriorityQueue<State>();
  const minCosts = new Map<State, number>();

  pq.push(initialState, 0);
  minCosts.set(initialState, 0);

  while (!pq.isEmpty()) {
    const { item: state, cost } = pq.pop()!;

    if (cost > (minCosts.get(state) ?? Infinity)) {
      continue; // Found a cheaper path already
    }

    if (state === targetState) {
      return cost; // Found the solution!
    }

    const moves = getValidMoves(state, roomDepth);

    for (const { nextState, moveCost } of moves) {
      const newCost = cost + moveCost;
      if (newCost < (minCosts.get(nextState) ?? Infinity)) {
        minCosts.set(nextState, newCost);
        pq.push(nextState, newCost);
      }
    }
  }

  return -1; // No solution found
}

export function part1(input: State) {
  const roomDepth = 2;
  const targetState: State = '..........._AABBCCDD';
  return solve(input, targetState, roomDepth);
}

export function part2(input: State) {
  const roomDepth = 4;
  const targetState: State = '..........._AAAABBBBCCCCDDDD';

  // Modify the initial state to insert the new lines
  const [hallway, rooms] = input.split('_');
  const roomA = rooms[0] + 'DD' + rooms[1];
  const roomB = rooms[2] + 'CB' + rooms[3];
  const roomC = rooms[4] + 'BA' + rooms[5];
  const roomD = rooms[6] + 'AC' + rooms[7];

  const part2State: State = `${hallway}_${roomA}${roomB}${roomC}${roomD}`;

  return solve(part2State, targetState, roomDepth);
}

function parseInput(inputString: string): State {
  const lines = inputString.trim().split('\n');
  const hallway = '...........';

  // Extract amphipods from the two room lines
  const line3 = lines[2].match(/[A-D]/g)!;
  const line4 = lines[3].match(/[A-D]/g)!;

  // Re-order them by room (column)
  const roomA = line3[0] + line4[0];
  const roomB = line3[1] + line4[1];
  const roomC = line3[2] + line4[2];
  const roomD = line3[3] + line4[3];

  return `${hallway}_${roomA}${roomB}${roomC}${roomD}`;
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
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
`;

  if (!useRealInput) inputString = testInput;

  const parsedInput = parseInput(inputString);

  console.log('part1:', part1(parsedInput));
  console.log('part2:', part2(parsedInput));
}

// await main(false);
await main();
