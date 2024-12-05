function correctedUpdate(rules, update) {
  let isCorrect = true;
  do {
    isCorrect = true;
    for (let rule of rules) {
      let [pageX, pageY] = rule;
      if (!update.includes(pageX) || !update.includes(pageY)) continue;
      let indexX = update.indexOf(pageX);
      let indexY = update.indexOf(pageY);
      if (indexX > indexY) {
        isCorrect = false;
        update.splice(indexX, 1, pageY);
        update.splice(indexY, 1, pageX);
      }
    }
  } while (isCorrect === false);
}

export function part1(input: string) {
  input = input.trim();
  let [rulesSection, updatesSection] = input.split('\n\n');

  let rules = rulesSection.split('\n').map(line => line.split('|').map(Number));
  let updates = updatesSection.split('\n').map(line => line.split(',').map(Number));

  let correctUpdates = updates.filter(update => {
    let isCorrect = true;
    for (let rule of rules) {
      let [pageX, pageY] = rule;
      if (!update.includes(pageX) || !update.includes(pageY)) continue;
      if (update.indexOf(pageX) > update.indexOf(pageY)) {
        isCorrect = false;
      }
    }
    return isCorrect;
  });
  return correctUpdates
    .map(update => update[Math.floor(update.length / 2)])
    .reduce((a, b) => a + b);
}

export function part2(input: string) {
  input = input.trim();
  let [rulesSection, updatesSection] = input.split('\n\n');

  let rules = rulesSection.split('\n').map(line => line.split('|').map(Number));
  let updates = updatesSection.split('\n').map(line => line.split(',').map(Number));

  let correctedUpdates = updates.filter((update, i) => {
    let isCorrect = true;
    for (let rule of rules) {
      let [pageX, pageY] = rule;
      if (!update.includes(pageX) || !update.includes(pageY)) continue;
      if (update.indexOf(pageX) > update.indexOf(pageY)) {
        isCorrect = false;
        // Correct it
        correctedUpdate(rules, update);
      }
    }
    return !isCorrect;
  });
  return correctedUpdates
    .map(update => update[Math.floor(update.length / 2)])
    .reduce((a, b) => a + b);
}

async function main(useRealInput = true) {
  let input =
    // @ts-expect-error: next-line
    typeof Bun !== 'undefined'
      ? // @ts-expect-error: next-line
        await Bun.file(`${import.meta.dir}/input.txt`).text()
      : // @ts-expect-error: next-line
      typeof Deno !== 'undefined'
      ? // @ts-expect-error: next-line
        await Deno.readTextFile(`${import.meta.dirname}/input.txt`)
      : '';

  let testInput = `
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
`;
  if (!useRealInput) input = testInput;

  console.log('part1:', part1(input));
  console.log('part2:', part2(input));
}

// main(false);
main();
