function parse(source) {
  return source
    .trim()
    .split('\n')
    .map(line => line.split('-'));
}

function buildNetworkGraph(relations) {
  const graph = new Map();

  relations.forEach(([a, b]) => {
    if (!graph.has(a)) {
      graph.set(a, new Set());
    }
    if (!graph.has(b)) {
      graph.set(b, new Set());
    }
    graph.get(a).add(b);
    graph.get(b).add(a);
  });

  return graph;
}

function localeComparator(a, b) {
  return a.localeCompare(b);
}

function intersection(a, b) {
  return new Set([...a].filter(value => b.has(value)));
}

function difference(a, b) {
  return new Set([...a].filter(value => !b.has(value)));
}

function union(a, b) {
  return new Set([...a, ...b]);
}

function findGroupsOfThree(graph, node) {
  const result = new Set();

  graph.get(node).forEach((neighbor, _, nodeNeighbors) => {
    intersection(nodeNeighbors, graph.get(neighbor)).forEach(commonNeighbor => {
      result.add([node, neighbor, commonNeighbor].sort(localeComparator).join(','));
    });
  });

  return result;
}

function findAllGroupsOfThree(graph) {
  return [...graph.keys()].reduce((groups, node) => {
    findGroupsOfThree(graph, node).forEach(group => groups.add(group));
    return groups;
  }, new Set());
}

function findLargestGroup(graph) {
  /**
   * Performs Randomized Selection to select a pivot vertex for the Bron-Kerbosh algorithm.
   */
  const choosePivot = (P, X) => {
    const candidates = [...union(P, X)];
    return candidates[Math.floor(Math.random()) * (candidates.length - 1)];
  };

  /**
   * An implementation of the Bron-Kerbosh algorithm with Pivoting.
   * @see {@link https://www.wikiwand.com/en/articles/Bron%E2%80%93Kerbosch_algorithm}
   */
  const findAllMaximalCliques = (
    N,
    R = new Set(),
    P = new Set(N.keys()),
    X = new Set(),
    result = []
  ) => {
    if (!P.size && !X.size) {
      result.push(R);
      return result;
    }

    const u = choosePivot(P, X);
    const Nu = graph.get(u);
    difference(P, Nu).forEach(v => {
      const Nv = graph.get(v);
      findAllMaximalCliques(
        N,
        union(R, new Set([v])),
        intersection(P, Nv),
        intersection(X, Nv),
        result
      );

      P.delete(v);
      X.add(v);
    });

    return result;
  };

  const cliques = findAllMaximalCliques(graph);
  const largestCliqueSize = Math.max(...cliques.map(clique => clique.size));
  return cliques.find(clique => clique.size === largestCliqueSize);
}

function part1(data) {
  const graph = buildNetworkGraph(data);
  return [...findAllGroupsOfThree(graph)].filter(g => g.match(/t[a-z]/)).length;
}

function part2(data) {
  const graph = buildNetworkGraph(data);
  return [...findLargestGroup(graph)].sort(localeComparator).join(',');
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
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
`;

  if (!useRealInput) inputString = testInput;

  let input = parse(inputString);
  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// await main(false);
await main();
