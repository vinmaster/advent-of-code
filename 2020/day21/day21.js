const fs = require('fs');
const path = require('path');

// util.inspect.defaultOptions.maxArrayLength = null;
// util.inspect.defaultOptions.showHidden = true;
// util.inspect.defaultOptions.depth = null;
// util.inspect.defaultOptions.compact = true;

function part1(input) {
  /** @type {string[]} */
  let lines = input
    .trim()
    .split('\n');

  let foods = [];
  let possibleAllergens = {};
  for (let line of lines) {
    let [ing, alg] = line.split(' (contains ');
    alg = alg.slice(0, -1);
    foods.push({
      ingredients: ing.split(' '),
      allergens: alg.split(', '),
    })
  }
  console.log(foods);

  // Combine without duplicates
  let combine = (arr1, arr2) => [...new Set(arr1.concat(arr2))];
  for (let food of foods) {
    let { ingredients, allergens } = food;
    for (let ing of ingredients) {
      if (!possibleAllergens[ing]) possibleAllergens[ing] = [];

      possibleAllergens[ing] = combine(possibleAllergens[ing], allergens);
    }
  }

  console.log(possibleAllergens);
  for (let [ing, alg] of Object.entries(possibleAllergens)) {
    if (alg.length === 1) {
      let target = alg[0];
      for (let [ing2, alg2] of Object.entries(possibleAllergens)) {
        if (ing !== ing2) {
          alg2.splice(alg2.indexOf(alg[0]), 1);
        }
      }
      break;
    }
  }
  console.log(possibleAllergens);

  let targetIngredients = [];
  for (let [ing, alg] of Object.entries(possibleAllergens)) {
    if (alg.length === 0) {
      targetIngredients.push(ing);
    }
  }

  console.log(targetIngredients);

  let count = 0;
  for (let { ingredients, allergens } of foods) {
    for (let i of ingredients) {
      if (targetIngredients.includes(i)) {
        count += 1;
      }
    }
  }
  return count;
}

function part2(input) {
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

input = `
mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
`

console.log('day21 part1:', part1(input));
console.log('day21 part2:', part2(input));
