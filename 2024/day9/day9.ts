type File = {
  id: number;
  start: number;
  size: number;
};

function findFiles(disk): File[] {
  const files = new Map();
  for (let i = 0; i < disk.length; i++) {
    const id = disk[i];
    if (id === '.') continue;

    if (!files.has(id)) {
      files.set(id, {
        id,
        start: i,
        size: 1,
      });
    } else {
      files.get(id).size++;
    }
  }
  return Array.from(files.values());
}

function findFreeSpaceIndex(disk, start, size) {
  let freeSpace = 0;
  let index = -1;
  for (let i = 0; i < start; i++) {
    if (disk[i] === '.') {
      if (index === -1) index = i;
      freeSpace++;
      if (freeSpace === size) return index;
    } else {
      freeSpace = 0;
      index = -1;
    }
  }
  return -1;
}

function moveFile(disk: any[], file: File, index) {
  for (let i = 0; i < file.size; i++, index += 1) {
    disk.splice(file.start + i, 1, '.');
    disk.splice(index, 1, file.id);
  }
}

export function part1(input: string) {
  input = input.trim();
  let disk = [] as (number | '.')[];

  for (let i = 0; i < input.length; i++) {
    let size = Number(input[i]);
    if (i % 2 === 0) {
      // Block
      disk = disk.concat(...Array(size).fill(i / 2));
    } else {
      // Free space
      disk = disk.concat(...Array(size).fill('.'));
    }
  }

  let firstFreeSpaceIndex = disk.indexOf('.');
  for (let i = disk.length - 1; i > 0 && firstFreeSpaceIndex < i; i--) {
    if (disk[i] !== '.') {
      let id = disk[i];
      disk.splice(i, 1, '.');
      disk.splice(firstFreeSpaceIndex, 1, id);
      firstFreeSpaceIndex = disk.indexOf('.');
      // console.log(disk.join(''));
    }
  }

  let sum = 0;
  for (let i = 0; i < firstFreeSpaceIndex; i++) {
    let id = disk[i];
    sum += i * Number(id);
  }
  return sum;
}

export function part2(input: string) {
  input = input.trim();
  let disk = [] as (number | '.')[];

  for (let i = 0; i < input.length; i++) {
    let size = Number(input[i]);
    if (i % 2 === 0) {
      // Block
      disk = disk.concat(...Array(size).fill(i / 2));
    } else {
      // Free space
      disk = disk.concat(...Array(size).fill('.'));
    }
  }

  let files = findFiles(disk);
  files.sort((a, b) => b.id - a.id);
  for (let file of files) {
    let freeSpaceIndex = findFreeSpaceIndex(disk, file.start, file.size);
    if (freeSpaceIndex !== -1) {
      moveFile(disk, file, freeSpaceIndex);
    }
  }
  // console.log(disk.join(''));

  let sum = 0;
  for (let i = 0; i < disk.length; i++) {
    let id = disk[i];
    if (id !== '.') sum += i * Number(id);
  }
  return sum;
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
2333133121414131402
`;
  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(inputString));
  console.log('part2:', part2(inputString));
}

// await main(false);
await main();
