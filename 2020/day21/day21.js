const fs = require('fs');
const path = require('path');

function parseInput(input) {
  return input
    .trim()
    .split('\n')
    .map(line => {
      const [ingredientsPart, allergensPart] = line.split(' (contains ');
      const ingredients = ingredientsPart.split(' ');
      const allergens = allergensPart ? allergensPart.slice(0, -1).split(', ') : [];
      return { ingredients, allergens };
    });
}

function part1(input) {
  const foods = parseInput(input);
  const allergenMap = new Map();
  for (const { ingredients, allergens } of foods) {
    for (const allergen of allergens) {
      if (!allergenMap.has(allergen)) {
        allergenMap.set(allergen, new Set(ingredients));
      } else {
        const current = allergenMap.get(allergen);
        allergenMap.set(allergen, new Set([...current].filter(i => ingredients.includes(i))));
      }
    }
  }
  const possiblyDangerous = new Set([...allergenMap.values()].flatMap(s => [...s]));
  let safeCount = 0;
  for (const { ingredients } of foods) {
    for (const i of ingredients) {
      if (!possiblyDangerous.has(i)) safeCount++;
    }
  }
  return safeCount.toString();
}

function part2(input) {
  const foods = parseInput(input);
  const allergenMap = new Map();

  for (const { ingredients, allergens } of foods) {
    for (const allergen of allergens) {
      if (!allergenMap.has(allergen)) {
        allergenMap.set(allergen, new Set(ingredients));
      } else {
        const current = allergenMap.get(allergen);
        allergenMap.set(allergen, new Set([...current].filter(i => ingredients.includes(i))));
      }
    }
  }

  const resolved = {};
  const unresolved = new Map([...allergenMap.entries()].map(([a, s]) => [a, new Set(s)]));
  while (unresolved.size > 0) {
    for (const [allergen, ingredients] of unresolved.entries()) {
      if (ingredients.size === 1) {
        const ingredient = [...ingredients][0];
        resolved[allergen] = ingredient;
        unresolved.delete(allergen);
        for (const other of unresolved.values()) {
          other.delete(ingredient);
        }
      }
    }
  }
  const canonical = Object.entries(resolved)
    .sort((a, b) => a[0].localeCompare(b[0]))
    .map(([_, ingredient]) => ingredient)
    .join(',');
  return canonical;
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

// Test example input
// input = `
// mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
// trh fvjkl sbzzf mxmxvkd (contains dairy)
// sqjhc fvjkl (contains soy)
// sqjhc mxmxvkd sbzzf (contains fish)
// `;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
