type Input = string[];
type Tree = {
  value?: number;
  parent?: Tree;
  left?: Tree;
  right?: Tree;
};

function toTree(json: any, parent?: Tree): Tree {
  if (typeof json === 'number') {
    return { value: json, parent };
  }

  let tree: Tree = { parent };
  tree.left = toTree(json[0], tree);
  tree.right = toTree(json[1], tree);
  return tree;
}

function toString(tree: Tree): string {
  if (tree.value !== undefined) return tree.value.toString();
  return `[${toString(tree.left!)},${toString(tree.right!)}]`;
}

function add(left: Tree, right: Tree): Tree {
  let tree: Tree = { left, right };
  left.parent = tree;
  right.parent = tree;

  return reduce(tree);
}

function reduce(tree: Tree): Tree {
  while (true) {
    if (!explode(tree) && !split(tree)) {
      break;
    }
  }
  return tree;
}

function explode(tree: Tree, depth = 0): boolean {
  if (tree.left && tree.right) {
    return explode(tree.left, depth + 1) || explode(tree.right, depth + 1);
  }
  if (depth > 4) {
    // Take left number to add to the nearest left number
    explodingLeft(tree.parent!);
    // Take right number to add to the nearest right number
    explodingRight(tree.parent!);

    tree.parent!.value = 0;
    tree.parent!.left = undefined;
    tree.parent!.right = undefined;
    return true;
  }
  return false;
}

function explodingLeft(tree: Tree) {
  let valueToAdd = tree.left!.value!;
  let node = tree;
  // Keep going up as long we are on left until we are on right
  while (node.parent && node.parent!.left === node) {
    node = node.parent!;
  }
  // Hit root, no number to add
  if (!node.parent) return;

  // We are on right, go to left
  let target = node.parent!.left;
  while (target && target.right) {
    target = target.right;
  }

  // Found a number to add
  if (target && target.value !== undefined) {
    target.value += valueToAdd;
  }
}

function explodingRight(tree: Tree) {
  let valueToAdd = tree.right!.value!;
  let node = tree;
  // Keep going up as long we are on right until we are on left
  while (node.parent && node.parent!.right === node) {
    node = node.parent!;
  }
  // Hit root, no number to add
  if (!node.parent) return;

  // We are on left, go to right
  let target = node.parent!.right;
  while (target && target.left) {
    target = target.left;
  }

  // Found a number to add
  if (target && target.value !== undefined) {
    target.value += valueToAdd;
  }
}

function split(tree: Tree): boolean {
  if (tree.left && tree.right) {
    return split(tree.left) || split(tree.right);
  }
  if (tree.value && tree.value >= 10) {
    let left: Tree = { value: Math.floor(tree.value / 2), parent: tree };
    let right: Tree = { value: Math.ceil(tree.value / 2), parent: tree };
    tree.value = undefined;
    tree.left = left;
    tree.right = right;
    return true;
  }
  return false;
}

function magnitude(tree: Tree): number {
  if (tree.value !== undefined) {
    return tree.value;
  }
  return 3 * magnitude(tree.left!) + 2 * magnitude(tree.right!);
}

export function part1(input: Input) {
  let snailfishNums = input.map(line => JSON.parse(line));
  let sum = snailfishNums.map(num => toTree(num)).reduce(add);
  return magnitude(sum);
}

export function part2(input: Input) {
  let largest = Number.NEGATIVE_INFINITY;
  let snailfishNums = input.map(line => JSON.parse(line));
  for (let i = 0; i < snailfishNums.length; i++) {
    for (let j = 0; j < snailfishNums.length; j++) {
      if (i === j) continue;
      let sum = add(toTree(snailfishNums[i]), toTree(snailfishNums[j]));
      if (magnitude(sum) > largest) largest = magnitude(sum);
    }
  }
  return largest;
}

function parseInput(inputString: string): Input {
  return inputString.trim().split('\n');
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
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
