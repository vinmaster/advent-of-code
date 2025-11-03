type Input = Scanner[];
type Scanner = { name: string; beacons: Vector[] };
type Vector = [number, number, number];

/**
 * Takes a 3D vector [x, y, z] and returns an array of all 24
 * possible 90-degree rotational orientations.
 *
 * @param {number[]} vector - An array [x, y, z]
 * @returns {number[][]} An array containing 24 orientation arrays.
 */
function getOrientations(vector: Vector): Vector[] {
  let [x, y, z] = vector;

  return [
    // Group 1: Facing +Z (z-axis is 3rd element, positive)
    // "Rolls" around the Z-axis
    [x, y, z],
    [-y, x, z],
    [-x, -y, z],
    [y, -x, z],

    // Group 2: Facing -Z (z-axis is 3rd element, negative)
    // "Rolls" around the Z-axis
    [x, -y, -z],
    [y, x, -z],
    [-x, y, -z],
    [-y, -x, -z],

    // Group 3: Facing +X (x-axis is 3rd element, positive)
    // "Rolls" around the X-axis
    [-z, y, x],
    [-y, -z, x],
    [z, -y, x],
    [y, z, x],

    // Group 4: Facing -X (x-axis is 3rd element, negative)
    // "Rolls" around the X-axis
    [z, y, -x],
    [-y, z, -x],
    [-z, -y, -x],
    [y, -z, -x],

    // Group 5: Facing +Y (y-axis is 3rd element, positive)
    // "Rolls" around the Y-axis
    [x, -z, y],
    [z, x, y],
    [-x, z, y],
    [-z, -x, y],

    // Group 6: Facing -Y (y-axis is 3rd element, negative)
    // "Rolls" around the Y-axis
    [x, z, -y],
    [-z, x, -y],
    [-x, -z, -y],
    [z, -x, -y],
  ];
}

function add(a: Vector, b: Vector): Vector {
  return [a[0] + b[0], a[1] + b[1], a[2] + b[2]];
}

function subtract(a: Vector, b: Vector): Vector {
  return [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
}

function manhattanDistance(a: Vector, b: Vector): number {
  return Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) + Math.abs(a[2] - b[2]);
}

function tryNormalizeScanner(
  knownBeacons: Vector[],
  otherBeacons: Vector[]
): { scannerPosition: Vector; normalizedBeacons: Vector[] } | undefined {
  const minMatchingCount = 12;
  // 1. Try all 24 possible orientations for normalizedBeacons
  for (let i = 0; i < 24; i++) {
    // Get all of normalizedBeacons's beacons in the i-th orientation
    const orientedBeacons = otherBeacons.map(b => getOrientations(b)[i]);
    // 2. Use a map to count potential "delta" vectors
    const deltas = new Map<string, number>();
    // 3. Compare every known beacon against every oriented beacon
    for (const known of knownBeacons) {
      for (const oriented of orientedBeacons) {
        // The delta is the hypothetical position of normalizedBeacons
        // relative to the known scanner's coordinate system.
        // delta = known_beacon_pos - oriented_beacon_pos
        const delta = subtract(known, oriented);
        const deltaKey = delta.join(',');
        const count = (deltas.get(deltaKey) || 0) + 1;
        deltas.set(deltaKey, count);

        // 4. Check for a match
        if (count >= minMatchingCount) {
          // We found it! This delta is the correct one.
          const scannerPosition = delta; // This IS the position of normalizedBeacons
          // 5. Normalize all of normalizedBeacons's beacons to world space
          // normalized_pos = oriented_pos + scanner_pos
          const normalizedBeacons = orientedBeacons.map(b => add(b, scannerPosition));
          return { scannerPosition, normalizedBeacons };
        }
      }
    }
  }
  return;
}

export function part1(input: Input): number {
  // This Set will store all unique beacon positions in "world space"
  // We use a string "x,y,z" as the key for easy uniqueness checks.
  const finalBeacons = new Set<string>();
  const scannerPositions: Vector[] = [];
  let pendingScanners = [...input];
  // A queue of scanners that are aligned.
  const scannersQueue: Scanner[] = [];
  const firstScanner = pendingScanners.shift()!;
  scannersQueue.push(firstScanner); // Add to queue (its beacons are "world" for now)
  scannerPositions.push([0, 0, 0]); // Its position is 0,0,0
  firstScanner.beacons.forEach(b => finalBeacons.add(b.join(',')));

  while (scannersQueue.length > 0) {
    const currentScanner = scannersQueue.shift()!;
    const remainingPending: Scanner[] = [];

    for (const scanner of pendingScanners) {
      const normalized = tryNormalizeScanner(currentScanner.beacons, scanner.beacons);
      if (normalized) {
        // Normalized 'pending'
        scannerPositions.push(normalized.scannerPosition);
        // Add its normalized (world-space) beacons to the final set
        normalized.normalizedBeacons.forEach(b => finalBeacons.add(b.join(',')));
        // Add this newly normalized scanner to the queue,
        scannersQueue.push({
          name: scanner.name,
          beacons: normalized.normalizedBeacons,
        });
      } else {
        // No match, add it back to the list for the next queue
        remainingPending.push(scanner);
      }
    }
    // Update the list of pending scanners
    pendingScanners = remainingPending;
  }

  return finalBeacons.size;
}

export function part2(input: Input) {
  const scannerPositions: Vector[] = [];
  let pendingScanners = [...input];
  // A queue of scanners that are aligned.
  const scannersQueue: Scanner[] = [];
  const firstScanner = pendingScanners.shift()!;
  scannersQueue.push(firstScanner); // Add to queue (its beacons are "world" for now)
  scannerPositions.push([0, 0, 0]); // Its position is 0,0,0

  while (scannersQueue.length > 0) {
    const currentScanner = scannersQueue.shift()!;
    const remainingPending: Scanner[] = [];

    for (const scanner of pendingScanners) {
      const normalized = tryNormalizeScanner(currentScanner.beacons, scanner.beacons);
      if (normalized) {
        // Normalized 'pending'
        scannerPositions.push(normalized.scannerPosition);
        // Add this newly normalized scanner to the queue,
        scannersQueue.push({
          name: scanner.name,
          beacons: normalized.normalizedBeacons,
        });
      } else {
        // No match, add it back to the list for the next queue
        remainingPending.push(scanner);
      }
    }
    // Update the list of pending scanners
    pendingScanners = remainingPending;
  }

  let largest = Number.NEGATIVE_INFINITY;
  for (let s1 of scannerPositions) {
    for (let s2 of scannerPositions) {
      if (s1.toString() !== s2.toString()) {
        let distance = manhattanDistance(s1, s2);
        if (distance > largest) largest = distance;
      }
    }
  }
  return largest;
}

function parseInput(inputString: string): Input {
  let scanners: Scanner[] = [];
  let scannerEntries = inputString.trim().split('\n\n');
  let count = 0;
  for (let scannerEntry of scannerEntries) {
    let beaconEntries = scannerEntry.split('\n').slice(1);
    scanners.push({
      name: `scanner ${count++}`,
      beacons: beaconEntries.map(e => e.split(',').map(Number) as Vector),
    });
  }

  return scanners;
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
--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14
`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
