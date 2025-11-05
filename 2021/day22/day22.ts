type Input = { isOn: boolean; ranges: number[] }[];

/**
 * A helper class to represent a 3D Cuboid for Part 2.
 * It handles calculating intersections and volumes.
 */
class Cuboid {
  x1: number;
  x2: number;
  y1: number;
  y2: number;
  z1: number;
  z2: number;

  constructor(x1: number, x2: number, y1: number, y2: number, z1: number, z2: number) {
    this.x1 = x1;
    this.x2 = x2;
    this.y1 = y1;
    this.y2 = y2;
    this.z1 = z1;
    this.z2 = z2;
  }

  volume(): bigint {
    // +1n because ranges are inclusive (e.g., 10..12 is 3 cubes)
    const dx = BigInt(this.x2 - this.x1 + 1);
    const dy = BigInt(this.y2 - this.y1 + 1);
    const dz = BigInt(this.z2 - this.z1 + 1);
    return dx * dy * dz;
  }

  /**
   * Finds the intersection of this cuboid with another.
   * @returns A new Cuboid representing the overlap, or null if they don't overlap.
   */
  intersection(other: Cuboid): Cuboid | null {
    const ix1 = Math.max(this.x1, other.x1);
    const ix2 = Math.min(this.x2, other.x2);
    const iy1 = Math.max(this.y1, other.y1);
    const iy2 = Math.min(this.y2, other.y2);
    const iz1 = Math.max(this.z1, other.z1);
    const iz2 = Math.min(this.z2, other.z2);
    if (ix1 <= ix2 && iy1 <= iy2 && iz1 <= iz2) {
      return new Cuboid(ix1, ix2, iy1, iy2, iz1, iz2);
    }
    // No overlap
    return null;
  }
}

export function part1(input: Input) {
  // This Set will store the string "x,y,z" for every cube that is "on".
  const onCubes = new Set<string>();
  for (const step of input) {
    const isOn = step.isOn;
    const [x1_raw, x2_raw, y1_raw, y2_raw, z1_raw, z2_raw] = step.ranges;
    const x1 = Math.max(-50, x1_raw);
    const x2 = Math.min(50, x2_raw);
    const y1 = Math.max(-50, y1_raw);
    const y2 = Math.min(50, y2_raw);
    const z1 = Math.max(-50, z1_raw);
    const z2 = Math.min(50, z2_raw);

    for (let x = x1; x <= x2; x++) {
      for (let y = y1; y <= y2; y++) {
        for (let z = z1; z <= z2; z++) {
          const key = `${x},${y},${z}`;
          // Add or remove the cube from our "on" set
          if (isOn) {
            onCubes.add(key);
          } else {
            onCubes.delete(key);
          }
        }
      }
    }
  }

  return onCubes.size;
}

export function part2(input: Input) {
  // This list will store all the final volumes to be added or subtracted.
  // The sign (1 or -1) implements the inclusion-exclusion principle.
  const regions: { cuboid: Cuboid; sign: 1 | -1 }[] = [];
  for (const step of input) {
    const [x1, x2, y1, y2, z1, z2] = step.ranges;
    const newCuboid = new Cuboid(x1, x2, y1, y2, z1, z2);
    // This temporary list will hold all intersections created by this new cuboid.
    const intersectionsToAdd: { cuboid: Cuboid; sign: 1 | -1 }[] = [];
    // Check for intersections with all previously processed regions
    for (const existingRegion of regions) {
      const intersection = newCuboid.intersection(existingRegion.cuboid);
      if (intersection) {
        // An intersection exists. We need to add a "correction" cuboid.
        // This new cuboid has the *opposite sign* of the one
        // it's intersecting with.
        intersectionsToAdd.push({
          cuboid: intersection,
          sign: existingRegion.sign === 1 ? -1 : 1,
        });
      }
    }
    // If the original command was "on", add the new cuboid itself
    // to the list as a "positive" region.
    if (step.isOn) {
      regions.push({ cuboid: newCuboid, sign: 1 });
    }
    // Add all the "correction" intersections we just found.
    // This step subtracts overlaps, or adds back doubly-subtracted overlaps.
    regions.push(...intersectionsToAdd);
  }
  let totalOnVolume = 0n; // Must use BigInt
  for (const region of regions) {
    const volume = region.cuboid.volume();
    if (region.sign === 1) {
      totalOnVolume += volume;
    } else {
      totalOnVolume -= volume;
    }
  }
  return totalOnVolume;
}

function parseInput(inputString: string): Input {
  const lineRegex = /(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/;
  let lines = inputString.trim().split('\n');
  let steps = [];
  for (let line of lines) {
    const match = line.match(lineRegex);
    if (!match) {
      console.error(`Failed to parse line: ${line}`);
      continue;
    }
    // match[1] is 'on' or 'off'
    // match[2]..match[7] are the coordinate strings
    const state = match[1];
    steps.push({
      isOn: state === 'on',
      ranges: match.slice(2).map(Number),
    });
  }
  return steps;
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
on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
