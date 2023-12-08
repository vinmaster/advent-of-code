const gcd = (a, b) => (b == 0 ? a : gcd(b, a % b));
const lcm = (a, b) => (a / gcd(a, b)) * b;
const lcmAll = ns => ns.reduce(lcm, 1);

function part1(input: string) {
  const lines = input.trim().split('\n');
  let instr = lines[0];
  let nodes = lines.slice(2).map(line => {
    let [name, left, right] = [...line.matchAll(/(\w+)/g)].map(m => m[0]);
    return { name, left, right };
  });
  let steps = 0;
  let currentInstr = 0;
  let current = 'AAA';
  while (current !== 'ZZZ') {
    let node = nodes.find(n => n.name === current);
    if (!node) throw new Error(`Node not found: ${current}`);
    current = instr[currentInstr] === 'L' ? node.left : node.right;
    currentInstr = (currentInstr + 1) % instr.length;
    steps += 1;
  }
  return steps;
}

function part2(input: string) {
  const lines = input.trim().split('\n');
  let instr = lines[0];
  let nodes = lines.slice(2).map(line => {
    let [name, left, right] = [...line.matchAll(/(\w+)/g)].map(m => m[0]);
    return { name, left, right };
  });
  let currents = nodes.filter(n => n.name.endsWith('A')).map(n => n.name);
  let steps = Array(currents.length).fill(0);
  for (let i = 0; i < currents.length; i++) {
    let current = currents[i];
    let currentInstr = 0;
    while (!current.endsWith('Z')) {
      let node = nodes.find(n => n.name === current);
      if (!node) throw new Error(`Node not found: ${current}`);
      current = instr[currentInstr] === 'L' ? node.left : node.right;
      currentInstr = (currentInstr + 1) % instr.length;
      steps[i] += 1;
    }
  }
  return lcmAll(steps);
}

let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
