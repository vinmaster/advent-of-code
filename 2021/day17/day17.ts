type Input = { xMin: number; xMax: number; yMin: number; yMax: number };
type Vector = { x: number; y: number };

function simulate(velocity: Vector, target: Input) {
  let position: Vector = { x: 0, y: 0 };
  let steps: Vector[] = [{ x: position.x, y: position.y }];

  while (position.x <= target.xMax && position.y >= target.yMin) {
    // Apply velocity
    position.x += velocity.x;
    position.y += velocity.y;
    // Drag
    velocity.x += -Math.sign(velocity.x);
    // Gravity
    velocity.y -= 1;
    steps.push({ x: position.x, y: position.y });
    if (
      position.x >= target.xMin &&
      position.x <= target.xMax &&
      position.y >= target.yMin &&
      position.y <= target.yMax
    ) {
      return steps;
    }
  }
  return;
}

export function part1(input: Input) {
  let maxY = 0;
  for (let vx = 0; vx <= input.xMax; vx++) {
    for (let vy = 1000; vy >= input.yMin; vy--) {
      let steps = simulate({ x: vx, y: vy }, input);
      if (steps) {
        let max = Math.max(...steps.map(pos => pos.y));
        if (max > maxY) maxY = max;
      }
    }
  }
  return maxY;
}

export function part2(input: Input) {
  let count = 0;
  for (let vx = 0; vx <= input.xMax; vx++) {
    for (let vy = 1000; vy >= input.yMin; vy--) {
      let steps = simulate({ x: vx, y: vy }, input);
      if (steps) {
        count += 1;
      }
    }
  }
  return count;
}

function parseInput(inputString: string): Input {
  let [xMin, xMax, yMin, yMax] = inputString
    .match(/.*=(\d+)\.\.(\d+).*=(-?\d+)\.\.(-?\d+).*/)!
    .slice(1)
    .map(Number);
  return { xMin, xMax, yMin, yMax };
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

  let testInput = `target area: x=20..30, y=-10..-5`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
