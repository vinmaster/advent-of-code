type Input = string[];

enum FieldType {
  Empty = '.',
  East = '>',
  South = 'v',
}

function step(map: FieldType[][]) {
  map = moveEast(map);
  map = moveSouth(map);

  return map;
}

function moveEast(map: FieldType[][]) {
  const newMap = map.map(_ => [] as FieldType[]);
  for (let i = 0; i < map.length; i++) {
    for (let j = 0; j < map[i].length; j++) {
      if (newMap[i][j]) {
        continue;
      }
      if (map[i][j] !== FieldType.East || map[i][(j + 1) % map[i].length] !== FieldType.Empty) {
        newMap[i][j] = map[i][j];
        continue;
      }
      newMap[i][j] = FieldType.Empty;
      newMap[i][(j + 1) % map[i].length] = map[i][j];
    }
  }
  return newMap;
}

function moveSouth(map: FieldType[][]) {
  const newMap = map.map(_ => [] as FieldType[]);
  for (let i = 0; i < map.length; i++) {
    for (let j = 0; j < map[i].length; j++) {
      if (newMap[i][j]) {
        continue;
      }
      if (map[i][j] !== FieldType.South || map[(i + 1) % map.length][j] !== FieldType.Empty) {
        newMap[i][j] = map[i][j];
        continue;
      }
      newMap[i][j] = FieldType.Empty;
      newMap[(i + 1) % map.length][j] = map[i][j];
    }
  }
  return newMap;
}

function compareArrays<T>(array1: T[][], array2: T[][]) {
  for (let i = 0; i < array1.length; i++) {
    for (let j = 0; j < array1[i].length; j++) {
      if (array1[i][j] !== array2[i][j]) {
        return false;
      }
    }
  }
  return true;
}

export function part1(input: Input) {
  const map = input.map(line => line.split('') as FieldType[]);

  let prevMap: FieldType[][] = map.map(y => y.map(x => FieldType.Empty));
  let currentMap = map;
  let i = 0;
  while (!compareArrays(prevMap, currentMap)) {
    prevMap = currentMap;
    currentMap = step(currentMap);
    i++;
  }

  return i.toString();
}

function parseInput(inputString: string): Input {
  return inputString.trim().split('\n').filter(Boolean);
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
          await Deno.readTextFile(new URL('./input.txt', import.meta.url).pathname)
        : '';
  } catch (error) {
    useRealInput = false;
  }

  if (inputString.length > 0) useRealInput = true;

  const parsedInput = parseInput(inputString);

  console.log('part1:', part1(parsedInput));
}

await main();

// Credit: https://github.com/adrbin/aoc-typescript/blob/main/2021/25/puzzle.ts
