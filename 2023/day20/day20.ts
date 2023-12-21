import util from 'util';

const gcd = (a, b) => (b == 0 ? a : gcd(b, a % b));
const lcm = (a, b) => (a / gcd(a, b)) * b;
const lcmAll = ns => ns.reduce(lcm, 1);

class Module {
  name: string;
  type: string;
  outputs: any[];
  memory: string | Record<string, string>;
  constructor(name, type, outputs) {
    this.name = name;
    this.type = type;
    this.outputs = outputs;
    if (type === '%') {
      this.memory = 'off';
    } else {
      this.memory = {};
    }
  }
  get [Symbol.toStringTag]() {
    return `${this}`;
  }
  [util.inspect.custom]() {
    return `${this}`;
  }
  toString() {
    return `${this.name}{type=${this.type},outputs=${this.outputs},memory=${JSON.stringify(
      this.memory
    )}}`;
  }
}

// Credit: https://www.youtube.com/watch?v=lxm6i21O83k
function part1(input: string) {
  let lines = input.trim().split('\n');
  let modules: Record<string, Module> = {};
  let broadcastTargets: any[] = [];

  for (let line of lines) {
    let [left, right] = line.split(' -> ');
    let outputs = right.split(', ');
    if (left === 'broadcaster') {
      broadcastTargets = outputs;
    } else {
      let [type, name] = [left[0], left.slice(1)];
      modules[name] = new Module(name, type, outputs);
    }
  }

  for (let [name, module] of Object.entries(modules)) {
    for (let output of module.outputs) {
      if (modules[output] !== undefined && modules[output].type === '&') {
        modules[output].memory[name] = 'lo';
      }
    }
  }

  let lo = 0;
  let hi = 0;
  for (let i = 0; i < 1000; i++) {
    lo += 1;
    let queue = broadcastTargets.map(b => ['broadcaster', b, 'lo']);
    while (queue.length > 0) {
      let [origin, target, pulse] = queue.shift()!;
      if (pulse === 'lo') lo += 1;
      else hi += 1;
      if (modules[target] === undefined) continue;
      let module = modules[target];
      if (module.type === '%') {
        if (pulse === 'lo') {
          if (module.memory === 'off') module.memory = 'on';
          else module.memory = 'off';
          let outgoing = 'hi';
          if (module.memory !== 'on') outgoing = 'lo';
          for (let x of module.outputs) {
            queue.push([module.name, x, outgoing]);
          }
        }
      } else if (module.type === '&') {
        module.memory[origin] = pulse;
        let outgoing = 'hi';
        if (Object.values(module.memory).every(m => m === 'hi')) {
          outgoing = 'lo';
        }
        for (let x of module.outputs) {
          queue.push([module.name, x, outgoing]);
        }
      } else {
        throw new Error('Invalid type: ' + module.type);
      }
    }
  }
  // console.log(modules);
  return lo * hi;
}

function part2(input: string) {
  let lines = input.trim().split('\n');
  let modules: Record<string, Module> = {};
  let broadcastTargets: any[] = [];

  for (let line of lines) {
    let [left, right] = line.split(' -> ');
    let outputs = right.split(', ');
    if (left === 'broadcaster') {
      broadcastTargets = outputs;
    } else {
      let [type, name] = [left[0], left.slice(1)];
      modules[name] = new Module(name, type, outputs);
    }
  }

  for (let [name, module] of Object.entries(modules)) {
    for (let output of module.outputs) {
      if (modules[output] !== undefined && modules[output].type === '&') {
        modules[output].memory[name] = 'lo';
      }
    }
  }

  let [feed] = Object.entries(modules)
    .filter(([_, module]) => module.outputs.includes('rx'))
    .map(m => m[0]);
  let seen: Record<string, number> = Object.entries(modules)
    .filter(([_, module]) => module.outputs.includes(feed))
    .map(m => m[0])
    .reduce((acc, name) => {
      acc[name] = 0;
      return acc;
    }, {});
  let cycleLengths = {};
  let presses = 0;
  for (let i = 0; i < 100000; i++) {
    let queue = broadcastTargets.map(b => ['broadcaster', b, 'lo']);
    presses += 1;
    while (queue.length > 0) {
      let [origin, target, pulse] = queue.shift()!;
      if (modules[target] === undefined) continue;
      let module = modules[target];
      if (module.name === feed && pulse === 'hi') {
        seen[origin] += 1;
        if (cycleLengths[origin] === undefined) {
          cycleLengths[origin] = presses;
        }
        if (Object.values(seen).every(x => x > 1)) {
          return lcmAll(Object.values(cycleLengths));
        }
      }
      if (module.type === '%') {
        if (pulse === 'lo') {
          if (module.memory === 'off') module.memory = 'on';
          else module.memory = 'off';
          let outgoing = 'hi';
          if (module.memory !== 'on') outgoing = 'lo';
          for (let x of module.outputs) {
            queue.push([module.name, x, outgoing]);
          }
        }
      } else if (module.type === '&') {
        module.memory[origin] = pulse;
        let outgoing = 'hi';
        if (Object.values(module.memory).every(m => m === 'hi')) {
          outgoing = 'lo';
        }
        for (let x of module.outputs) {
          queue.push([module.name, x, outgoing]);
        }
      } else {
        throw new Error('Invalid type: ' + module.type);
      }
    }
  }
  // console.log(modules);
}

// @ts-ignore: next-line
let input = await Bun.file(`${import.meta.dir}/input.txt`).text();

let testInput = `
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
`;
// input = testInput;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
