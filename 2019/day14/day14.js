const fs = require('fs');
const path = require('path');

/**
 * @type {Chemical[]}
 */
let leftovers = {};
// Set to true to see breakdown
let debug = false;

/**
 * @typedef {Object} Chemical
 * @property {number} qty - Quantity of the chemical
 * @property {string} name - Name of the chemical
 */

/**
 * @param {string} str
 * @return {Chemical}
 */
function parseChemicalStr(str) {
  let [qty, name] = str.split(' ');
  return { qty: parseInt(qty, 10), name };
}

/**
 * Calculate required chemicals
 * @param {Object.<string, string[]>} mapping - Mapping rules for reactions
 * @param {...Chemical} targetChemical - Chemical to calculate
 */
function getRequiredChemicals(mapping, targetChemical, depth = 0) {
  if (debug) console.log(''.padStart(depth, ' '), 'Need', targetChemical.qty, targetChemical.name);

  if (targetChemical.name === 'ORE') return targetChemical.qty;

  let entry = Object.entries(mapping).filter(([output, inputs]) => parseChemicalStr(output).name === targetChemical.name);

  if (entry.length === 0) throw new Error('Entry zero ' + targetChemical.name);
  entry = entry[0];

  let [output, inputs] = entry;
  output = parseChemicalStr(output);

  if (!leftovers[targetChemical.name]) leftovers[targetChemical.name] = 0;

  // Cover by leftovers
  if (targetChemical.qty <= leftovers[targetChemical.name]) {
    if (debug) console.log(''.padStart(depth, ' '), 'Covered by leftovers', leftovers);
    leftovers[targetChemical.name] -= targetChemical.qty;
    return 0;
  }

  // Add leftovers
  targetChemical.qty -= leftovers[targetChemical.name];
  leftovers[targetChemical.name] = 0;

  // Check how much it requires
  let uses = Math.ceil(targetChemical.qty / output.qty);
  leftovers[targetChemical.name] += (output.qty * uses) - targetChemical.qty;
  // console.log(''.padStart(depth, ' '), 'leftovers', leftovers);

  if (debug) console.log(''.padStart(depth, ' '), 'Requires', uses, 'reactions of', output.name, 'to produce', uses * output.qty);

  if (debug) console.log(''.padStart(depth, ' '), 'Leftover', leftovers[targetChemical.name], targetChemical.name);

  // Loop dependencies
  let totalChemicals = 0;
  for (let input of inputs) {
    let next = parseChemicalStr(input);
    next.qty *= uses
    if (debug) console.log(''.padStart(depth, ' '), 'Gather from', next.qty, next.name);
    totalChemicals += getRequiredChemicals(mapping, next, depth + 1);
  }
  return totalChemicals;
}

function part1(input) {
  let lines = input
    .trim()
    .split('\n');

  let outputRegex = /^(.+) => (\d+) (\w+)$/;
  let chemicalsRegex = /(\d+) (\w+)/g;
  let mapping = {};

  for (let line of lines) {
    let [_, inputs, outputNum, outputName] = line.match(outputRegex);

    mapping[`${outputNum} ${outputName}`] = inputs.match(chemicalsRegex);
  }

  let target = parseChemicalStr('1 FUEL');
  let result = getRequiredChemicals(mapping, target);

  return result;
}

function part2(input) {
  let lines = input
    .trim()
    .split('\n');

  let outputRegex = /^(.+) => (\d+) (\w+)$/;
  let chemicalsRegex = /(\d+) (\w+)/g;
  let mapping = {};

  for (let line of lines) {
    let [_, inputs, outputNum, outputName] = line.match(outputRegex);

    mapping[`${outputNum} ${outputName}`] = inputs.match(chemicalsRegex);
  }

  let maxOres = 1000000000000;
  let target = parseChemicalStr(`1 FUEL`);
  let ores = getRequiredChemicals(mapping, target);

  let min = Math.floor(maxOres / ores);
  let diff = ((min * 2) - min) / 2;
  let fuel = min;

  while (true) {
    leftovers = {};
    let target = parseChemicalStr(`${fuel} FUEL`);
    let ores = getRequiredChemicals(mapping, target);
    if (ores > maxOres) {
      fuel -= diff;
    }
    else if (ores < maxOres) {
      fuel += diff;
    }
    if (diff === 1) return Math.floor(fuel) - 1;
    diff = Math.ceil(diff / 2);
  }
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

console.log('day14 part1:', part1(input));
console.log('day14 part2:', part2(input));

/*
class Reaction {
  constructor(formula) {
    this.reset();
    var split, subsplit, a;
    this.reagents = [];
    this.formula = formula;
    split = formula.match(/(.*)\s=>\s(\d+)\s([A-Z]+)/);
    this.product = (new Component(split[3], parseInt(split[2])));
    split = split[1].split(', ');
    for (a = 0; a < split.length; a++) {
      subsplit = split[a].match(/(\d+)\s([A-Z]+)/);
      this.reagents.push(new Component(subsplit[2], parseInt(subsplit[1])));
    }
  }

  addReqQty(qty) {
    var prevreq;

    prevreq = this.totreq;
    this.req += qty;
    this.totreq = Math.ceil(this.req / this.product.qty) * this.product.qty;
    this.waste = this.totreq - this.req;

    return this.totreq - prevreq;
  }

  reset() {
    this.req = 0;
    this.totreq = 0;
    this.waste = 0;
  }
}

class Component {
  constructor(molecule, qty) {
    this.molecule = molecule;
    this.qty = qty;
  }
}

function oreReq(molecule, reqqty) {
  var retval = 0;
  var ix;
  var pqty;
  var r;
  var a;

  ix = prodix.indexOf(molecule);
  pqty = reactions[ix].product.qty;
  reqqty = reactions[ix].addReqQty(reqqty);

  for (a = 0; a < reactions[ix].reagents.length; a++) {
    r = reactions[ix].reagents[a];
    if (r.molecule == 'ORE') {
      retval += r.qty * (reqqty / pqty);
    } else {
      retval += oreReq(r.molecule, r.qty * (reqqty / pqty))
    };
  }
  return retval;
}

var data = input.toString().split('\n');
if (data[data.length - 1] == '') { data.pop(); }

var ret;
var reactions = [];
var prodix = [];

for (i = 0; i < data.length; i++) {
  reactions.push(new Reaction(data[i]));
  prodix.push(reactions[i].product.molecule)
}

ret = oreReq('FUEL', 1);
console.log('Part 1: ' + ret);

//PART 2
var maxore = 1000000000000;
var inc = maxore / 100;
var fuel = Math.ceil(maxore / ret);

while (inc >= 1) {
  ore = 0;
  while (ore < maxore) {
    fuel += inc;
    //reset quantities
    for (i = 0; i < reactions.length; i++) {
      reactions[i].reset();
    }

    ore = oreReq('FUEL', fuel);
  }
  fuel -= inc;
  inc = inc / 10;
}

ret = fuel;

console.log('Part 2: ' + ret);
*/