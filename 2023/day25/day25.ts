// Credit: https://github.com/rkeytacked/adventofcode-2023/blob/main/src/day-25.js
const e = (a, b) => (a < b ? a + ':' + b : b + ':' + a);
/**
 * @param {Map<string,Set<string>>} G
 * @param {Array<[string,string]>} E
 * @return {[number,Array<number>]}
 */
function kargerAlgorithm(G, E): [number, number] {
  const graph = new Map<string, Set<string>>([...G].map(([k, v]) => [k, new Set(v)]));
  const edges = E.slice();
  const edgeMultiplier = new Map<string, number>(edges.map(([a, b]) => [e(a, b), 1]));
  const nodeCount = new Map([...G.keys()].map(n => [n, 1]));

  function contract(u, v) {
    if (!graph.has(u) || !graph.has(v)) {
      return;
    }
    for (let vertex of graph.get(v)!) {
      if (vertex !== u) {
        edges.push(u < vertex ? [u, vertex] : [vertex, u]);
        graph.get(u)!.add(vertex);
        graph.get(vertex)!.delete(v);
        graph.get(vertex)!.add(u);
        edgeMultiplier.set(
          e(u, vertex),
          (edgeMultiplier.get(e(u, vertex)) || 0) + edgeMultiplier.get(e(vertex, v))!
        );
      }
    }
    graph.get(u)!.delete(v);
    graph.get(u)!.delete(u);
    graph.delete(v);
    edgeMultiplier.delete(e(u, v));
    nodeCount.set(u, nodeCount.get(u)! + nodeCount.get(v)!);
    nodeCount.delete(v);
  }

  function getRandomEdge() {
    const index = Math.floor(Math.random() * edges.length);
    const edge = edges[index];
    const last = edges.pop();
    if (index < edges.length) {
      edges[index] = last;
    }
    return edge;
  }

  while (graph.size > 2) {
    if (!edges.length) throw new Error(graph.size.toString());
    const [u, v] = getRandomEdge();
    contract(u, v);
  }
  return [edgeMultiplier.get(e(...graph.keys())), [...nodeCount.values()]];
}

function part1(input: string) {
  let lines = input
    .trim()
    .split('\n')
    .map(line => line.split(/:?\s+/));
  const GRAPH = new Map();
  const EDGES: [string, string][] = [];
  lines.forEach(([from, ...nodes]) =>
    nodes.forEach(to => {
      EDGES.push([from, to]);
      GRAPH.has(from) || GRAPH.set(from, new Set());
      GRAPH.get(from).add(to);
      GRAPH.has(to) || GRAPH.set(to, new Set());
      GRAPH.get(to).add(from);
    })
  );

  let solution;
  for (let i = 0; i < 1000; i++) {
    const [cut, sizes] = kargerAlgorithm(GRAPH, EDGES);
    // console.log('trial', i, `found ${cut}-cut`);
    if (cut <= 3) {
      solution = sizes;
      break;
    }
  }
  return solution.reduce((a, b) => a * b);
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
`;
// input = testInput;

console.log('part1:', part1(input));
