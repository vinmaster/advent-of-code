const fs = require('fs');
const path = require('path');

class Vector {
  x: number;
  y: number;
  z: number;

  constructor(x = 0, y = 0, z = 0) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  add(v: Vector) {
    this.x += v.x;
    this.y += v.y;
    this.z += v.z;
  }

  toString() {
    return `(${this.x}, ${this.y}, ${this.z})`;
  }
}

class Moon {
  name: string;
  pos: Vector;
  vec: Vector;

  constructor(name: string, pos = new Vector(), vec = new Vector()) {
    this.name = name;
    this.pos = pos;
    this.vec = vec;
  }

  pulls(other: Moon) {
    const getVecChange = (v1: Vector, v2: Vector): Vector => {
      let change = new Vector();

      if (v1.x > v2.x) change.x += 1;
      else if (v1.x < v2.x) change.x -= 1;
      if (v1.y > v2.y) change.y += 1;
      else if (v1.y < v2.y) change.y -= 1;
      if (v1.z > v2.z) change.z += 1;
      else if (v1.z < v2.z) change.z -= 1;

      return change;
    }
    let thisChange = getVecChange(other.pos, this.pos);
    let otherChange = getVecChange(this.pos, other.pos);
    this.vec.add(thisChange);
    other.vec.add(otherChange);
  }

  move() {
    this.pos.add(this.vec);
  }

  get potentialEnergy(): number {
    return [this.pos.x, this.pos.y, this.pos.z]
      .map(Math.abs)
      .reduce((n, sum) => n + sum);
  }

  get kineticEnergy(): number {
    return [this.vec.x, this.vec.y, this.vec.z]
      .map(Math.abs)
      .reduce((n, sum) => n + sum);
  }

  get totalEnergy(): number {
    return this.potentialEnergy * this.kineticEnergy;
  }

  toString(): string {
    let str = '';
    str += `name: ${this.name}\n`;
    str += `pos: ${this.pos.toString()} vec: ${this.vec.toString()}`;
    return str;
  }
}

class System {
  moons: Moon[];

  constructor(moons: Moon[]) {
    this.moons = moons;
  }

  applyGravity() {
    for (let a = 0; a < this.moons.length; a++) {
      for (let b = a + 1; b < this.moons.length; b++) {
        let m1 = this.moons[a];
        let m2 = this.moons[b];
        if (m1.name !== m2.name) {
          m1.pulls(m2);
        }
      }
    }
  }

  applyVelocity() {
    for (let m of this.moons) {
      m.move();
    }
  }

  step(n) {
    for (let s = 0; s < n; s++) {
      this.applyGravity();
      this.applyVelocity();
    }
  }

  get totalEnergy(): number {
    return this.moons.reduce((acc, m) => acc + m.totalEnergy, 0);
  }
}

let part1 = (input: string) => {
  let lines: string[] = input
    .trim()
    .split('\n');

  let positions = lines.map(l => {
    let [_, x, y, z] = l
      .match(/<x=(-?\d+), y=(-?\d+), z=(-?\d+)>/)
      .map(s => parseInt(s));
    return new Vector(x, y, z);
  });
  let moons = [
    new Moon('Io', positions[0]),
    new Moon('Europa', positions[1]),
    new Moon('Ganymede', positions[2]),
    new Moon('Callisto', positions[3]),
  ];

  let system = new System(moons);
  system.step(1000);

  // for (let m of system.moons) {
  //   console.log(m.toString());
  // }

  return system.totalEnergy;
}

let part2 = (input) => {
  let lines: string[] = input
    .trim()
    .split('\n');

  const gcd = (a, b) => {
    if (b === 0) return a;
    return gcd(b, a % b);
  };

  const _lcm = (a, b) => {
    if (b === 0) return 0;
    return (a * b) / gcd(a, b);
  };

  // @link https://stackoverflow.com/a/147523/864233
  const lcm = (...args) => args.reduce((a, b) => _lcm(a, b));

  type MoonDims = Moon[]

  interface Moon {
    initP: number;
    p: number;
    v: number;
  }

  let applyGravity = (dimension: MoonDims) => {
    for (let a = 0; a < dimension.length; a++) {
      for (let b = a + 1; b < dimension.length; b++) {
        let m1 = dimension[a];
        let m2 = dimension[b];
        if (m1.p > m2.p) { m1.v--; m2.v++; }
        else if (m1.p < m2.p) { m1.v++; m2.v--; }
      }
    }
  };

  let applyVelocity = (dimension: MoonDims) => {
    for (let d of dimension) {
      d.p += d.v;
    }
  }

  let isAtInit = (dimension: MoonDims) => {
    return dimension.every(m => m.initP === m.p && m.v === 0);
  }

  let matchingStep = [];

  let positions = lines.map(l => {
    let [_, x, y, z] = l
      .match(/<x=(-?\d+), y=(-?\d+), z=(-?\d+)>/)
      .map(s => parseInt(s));
    return [x, y, z];
  });

  let moonsX: MoonDims = positions.map(p => ({ initP: p[0], p: p[0], v: 0 }));
  let moonsY: MoonDims = positions.map(p => ({ initP: p[1], p: p[1], v: 0 }));
  let moonsZ: MoonDims = positions.map(p => ({ initP: p[2], p: p[2], v: 0 }));

  let moonDimensions = [moonsX, moonsY, moonsZ];
  for (const dim of moonDimensions) {
    let step = 0;
    let atInit = false;

    while (step < 1_000_000 && !atInit) {
      applyGravity(dim);
      applyVelocity(dim);
      step++;
      if (isAtInit(dim)) {
        matchingStep.push(step)
        atInit = true;
      }
    }
  }

  // console.log(matchingStep);

  return lcm(...matchingStep);
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day12 part1:', part1(input));
console.log('day12 part2:', part2(input));
