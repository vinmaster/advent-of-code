// deno-lint-ignore-file prefer-const

// credit: https://github.com/tymscar/Advent-Of-Code/blob/master/2022/typescript/day16/part2.ts

function part1(input: string) {
  let rooms: Room[] = input
    .trim()
    .split('\n')
    .filter(line => line.length > 0)
    .map(l => {
      const line = l.split(' ');
      const name = line[1];
      const adj = line.slice(9).map(v => v.replace(',', ''));
      const flow = Number(line[4].split('=')[1].slice(0, -1));
      return {
        name: name,
        flow: flow,
        adj: adj,
      } as Room;
    });

  const inputRooms: InputRoomMap = rooms.reduce(
    (map, currRoom) => ({
      ...map,
      [currRoom.name]: currRoom,
    }),
    {} as InputRoomMap
  );

  const startingRoom = inputRooms['AA'];

  const startingRooms = [startingRoom, ...rooms.filter(room => room.flow != 0)];
  const destinationRooms = rooms.filter(room => room.flow != 0);

  const roomsMovePrices: PricesRoomMap = startingRooms.reduce((map: PricesRoomMap, room: Room) => {
    return {
      ...map,
      [room.name]: dijkstra(
        room,
        destinationRooms.filter(r => r.name != room.name),
        inputRooms
      ),
    };
  }, {} as PricesRoomMap);

  return getMaxPressure(30, destinationRooms, roomsMovePrices, inputRooms);
}

function part2(input: string) {
  const rooms: Room[] = input
    .trim()
    .split('\n')
    .filter(line => line.length > 0)
    .map(l => {
      const line = l.split(' ');
      const name = line[1];
      const adj = line.slice(9).map(v => v.replace(',', ''));
      const flow = Number(line[4].split('=')[1].slice(0, -1));
      return {
        name: name,
        flow: flow,
        adj: adj,
      } as Room;
    });

  const inputRooms: InputRoomMap = rooms.reduce(
    (map, currRoom) => ({
      ...map,
      [currRoom.name]: currRoom,
    }),
    {} as InputRoomMap
  );

  const startingRoom = inputRooms['AA'];

  const startingRooms = [startingRoom, ...rooms.filter(room => room.flow != 0)];
  const destinationRooms = rooms.filter(room => room.flow != 0);

  const roomsMovePrices: PricesRoomMap = startingRooms.reduce((map: PricesRoomMap, room: Room) => {
    return {
      ...map,
      [room.name]: dijkstra(
        room,
        destinationRooms.filter(r => r.name != room.name),
        inputRooms
      ),
    };
  }, {} as PricesRoomMap);

  return getMostPressureReleaseWithElephant(destinationRooms, roomsMovePrices, inputRooms);
}

let input = await Deno.readTextFile(`${new URL('.', import.meta.url).pathname}input.txt`);

// input = `
// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
// Valve BB has flow rate=13; tunnels lead to valves CC, AA
// Valve CC has flow rate=2; tunnels lead to valves DD, BB
// Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
// Valve EE has flow rate=3; tunnels lead to valves FF, DD
// Valve FF has flow rate=0; tunnels lead to valves EE, GG
// Valve GG has flow rate=0; tunnels lead to valves FF, HH
// Valve HH has flow rate=22; tunnel leads to valve GG
// Valve II has flow rate=0; tunnels lead to valves AA, JJ
// Valve JJ has flow rate=21; tunnel leads to valve II`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));

type Room = {
  name: string;
  flow: number;
  adj: string[];
};

type InputRoomMap = {
  [name: string]: Room;
};

type CostMap = {
  [name: string]: number;
};

type PricesRoomMap = {
  [name: string]: CostMap;
};

type Path = {
  curr: string;
  toVisit: string[];
  timeLeft: number;
  finished: boolean;
  steps: string[];
  finalPressure: number;
};

function sortPathByPressure(a: Path, b: Path) {
  return b.finalPressure - a.finalPressure;
}

function getMaxPressure(
  time: number,
  destinations: Room[],
  priceMap: PricesRoomMap,
  roomsFromName: InputRoomMap
) {
  const paths: Path[] = [
    {
      curr: 'AA',
      toVisit: destinations.map(r => r.name),
      timeLeft: time,
      finished: false,
      steps: [],
      finalPressure: 0,
    },
  ];

  for (let n = 0; n < paths.length; n++) {
    const path = paths[n];
    if (path.timeLeft <= 0 || path.finished) {
      path.finished = true;
      continue;
    }

    const currPrices = priceMap[path.curr];
    let madeNewPath = false;
    path.toVisit.forEach(room => {
      if (room !== path.curr && path.timeLeft - currPrices[room] > 1) {
        madeNewPath = true;
        paths.push({
          curr: room,
          toVisit: path.toVisit.filter(v => v != room),
          timeLeft: path.timeLeft - currPrices[room] - 1,
          finished: false,
          steps: [...path.steps, room],
          finalPressure:
            path.finalPressure + (path.timeLeft - currPrices[room] - 1) * roomsFromName[room].flow,
        });
      }
    });
    if (!madeNewPath) path.finished = true;
  }

  return paths.filter(p => p.finished).sort(sortPathByPressure)[0].finalPressure;
}

function dijkstra(start: Room, endPositions: Room[], map: InputRoomMap): CostMap {
  const visited: string[] = [];
  const toVisit: Room[] = [start];
  const lowestCost: CostMap = {
    [start.name]: 0,
  };

  let curr;
  while ((curr = toVisit.shift())) {
    if (visited.includes(curr.name)) continue;

    const worthItAdj = curr.adj
      .filter(neighbour => !visited.includes(neighbour))
      .map(neighbour => map[neighbour]);

    toVisit.push(...worthItAdj);

    const costToCurr = lowestCost[curr.name];

    worthItAdj.forEach(neighbour => {
      const newCostToNeighbour = costToCurr + 1;
      const costToNeighbour =
        lowestCost[neighbour.name] === undefined ? newCostToNeighbour : lowestCost[neighbour.name];

      if (newCostToNeighbour <= costToNeighbour) {
        lowestCost[neighbour.name] = newCostToNeighbour;
      }
    });

    visited.push(curr.name);
  }

  return endPositions.reduce((map: CostMap, room: Room) => {
    return {
      ...map,
      [room.name]: lowestCost[room.name],
    };
  }, {});
}

function getMostPressureReleaseWithElephant(
  destinationRooms: Room[],
  roomsMovePrices: PricesRoomMap,
  inputRooms: InputRoomMap
) {
  let mostPressureReleased = -1;
  const paths = getAllPaths(26, destinationRooms, roomsMovePrices, inputRooms);
  for (let humanPath = 0; humanPath < paths.length; humanPath++)
    for (let elephantPath = humanPath + 1; elephantPath < paths.length; elephantPath++)
      if (paths[humanPath].steps.every(s => !paths[elephantPath].steps.includes(s)))
        if (
          paths[humanPath].finalPressure + paths[elephantPath].finalPressure >
          mostPressureReleased
        )
          mostPressureReleased = paths[humanPath].finalPressure + paths[elephantPath].finalPressure;
  return mostPressureReleased;
}

function getAllPaths(
  time: number,
  destinations: Room[],
  priceMap: PricesRoomMap,
  roomsFromName: InputRoomMap
) {
  const paths: Path[] = [
    {
      curr: 'AA',
      toVisit: destinations.map(r => r.name),
      timeLeft: time,
      finished: false,
      steps: [],
      finalPressure: 0,
    },
  ];

  for (let n = 0; n < paths.length; n++) {
    const path = paths[n];
    if (path.timeLeft <= 0 || path.finished) {
      path.finished = true;
      continue;
    }

    const currPrices = priceMap[path.curr];
    let madeNewPath = false;
    path.toVisit.forEach(room => {
      if (room !== path.curr && path.timeLeft - currPrices[room] > 1) {
        madeNewPath = true;
        const newTimeAfterVisitAndValveOpen = path.timeLeft - currPrices[room] - 1;
        paths.push({
          curr: room,
          toVisit: path.toVisit.filter(v => v != room),
          timeLeft: newTimeAfterVisitAndValveOpen,
          finished: false,
          steps: [...path.steps, room],
          finalPressure:
            path.finalPressure + newTimeAfterVisitAndValveOpen * roomsFromName[room].flow,
        });
        paths.push({
          curr: room,
          toVisit: [],
          timeLeft: newTimeAfterVisitAndValveOpen,
          finished: true,
          steps: [...path.steps, room],
          finalPressure:
            path.finalPressure + newTimeAfterVisitAndValveOpen * roomsFromName[room].flow,
        });
      }
    });
    if (!madeNewPath) path.finished = true;
  }

  return paths.filter(p => p.finished).sort(sortPathByPressure);
}

/*
Source: https://mcc.id.au/2004/10/dijkstra.js
function shortestPath(edges: Matrix, startVertex: string) {
  let numVertices = Object.keys(edges).length;
  let done = {} as Record<string, boolean>;
  done[startVertex] = true;
  let pathLengths = {} as Record<string, number>;
  let predecessors = {} as Record<string, string>;
  for (let i of Object.keys(edges)) {
    pathLengths[i] = edges[startVertex][i] ?? -Infinity;
    if (edges[startVertex][i] != -Infinity) {
      predecessors[i] = startVertex;
    }
  }
  pathLengths[startVertex] = 0;
  for (let i of Object.keys(edges)) {
    let closest!: string;
    let closestDistance = -Infinity;
    for (let j of Object.keys(edges)) {
      if (!done[j] && pathLengths[j] > closestDistance) {
        closestDistance = pathLengths[j];
        closest = j;
      }
    }
    done[closest] = true;
    for (let j of Object.keys(edges)) {
      if (!done[j]) {
        let possiblyCloserDistance = pathLengths[closest] + edges[closest][j];
        if (possiblyCloserDistance > pathLengths[j]) {
          pathLengths[j] = possiblyCloserDistance;
          predecessors[j] = closest;
        }
      }
    }
  }
  return { startVertex, pathLengths, predecessors };
}

function constructPath(shortestPathInfo: any, endVertex: any) {
  let path = [];
  while (endVertex != shortestPathInfo.startVertex) {
    path.unshift(endVertex);
    endVertex = shortestPathInfo.predecessors[endVertex];
  }
  return path;
}
*/

/*
// Source: https://www.algorithms-and-technologies.com/dijkstra/javascript
function dijkstra(graph: Matrix, start: string) {
  //This contains the distances from the start node to all other nodes
  let distances = {} as Record<string, number>;
  //path for each of the key
  let paths = {} as Record<string, string[]>;
  //Initializing with a distance of "Infinity"
  for (let key of Object.keys(graph)) {
    distances[key] = Infinity;
    paths[key] = [];
  }
  //The distance from the start node to itself is of course 0
  distances[start] = 0;
  //This contains whether a node was already visited
  let visited = {} as Record<string, boolean>;
  //While there are nodes left to visit...
  while (true) {
    // ... find the node with the currently shortest distance from the start node...
    let shortestDistance = Infinity;
    let shortestIndex: string | null = null;
    for (let key of Object.keys(graph)) {
      //... by going through all nodes that haven't been visited yet
      if (distances[key] < shortestDistance && !visited[key]) {
        shortestDistance = distances[key];
        shortestIndex = key;
      }
    }
    // console.log('Visiting node ' + shortestIndex + ' with current distance ' + shortestDistance);
    if (!shortestIndex) {
      // There was no node not yet visited --> We are done
      return { distances, paths };
    }
    //...then, for all neighboring nodes....
    for (let key of Object.keys(graph)) {
      //...if the path over this edge is shorter...
      if (
        graph[shortestIndex][key] !== undefined &&
        distances[key] > distances[shortestIndex] + graph[shortestIndex][key]
      ) {
        //...Save this path as new shortest path.
        distances[key] = distances[shortestIndex] + graph[shortestIndex][key];
        // paths[key].push(shortestIndex);
        // paths[shortestIndex].push(key);
        // console.log('Updating distance of node ' + key + ' to ' + distances[key]);
      }
    }
    // Lastly, note that we are finished with this node.
    visited[shortestIndex] = true;
    // console.log('Visited nodes: ', visited);
    // console.log('Currently lowest distances: ', distances);
  }
}
*/
