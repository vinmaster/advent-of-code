const fs = require('fs');
const path = require('path');

function parsePassports(lines) {
  const passports = [{}];
  for (const line of lines) {
    if (line.length === 0) {
      passports.push({});
      continue;
    }
    const entries = line.split(' ');
    const lastPassport = passports[passports.length - 1];
    for (const entry of entries) {
      const [key, value] = entry.split(':')
      lastPassport[key] = value;
    }
  }
  return passports;
}

function part1(input) {
  const lines = input
    .trim()
    .split('\n');
  const requiredFields = [
    'byr',
    'iyr',
    'eyr',
    'hgt',
    'hcl',
    'ecl',
    'pid',
  ];
  const passports = parsePassports(lines);

  const validPassports = passports.filter(p => {
    const keys = Object.keys(p);
    for (const field of requiredFields) {
      if (!keys.includes(field)) return false;
    }
    return true;
  })
  return validPassports.length;
}

function part2(input) {
  const lines = input
    .trim()
    .split('\n');
  const requiredFields = {
    'byr': (value) => parseInt(value) >= 1920 && parseInt(value) <= 2002,
    'iyr': (value) => parseInt(value) >= 2010 && parseInt(value) <= 2020,
    'eyr': (value) => parseInt(value) >= 2020 && parseInt(value) <= 2030,
    'hgt': (value) => {
      const [, num, unit] = value.match(/^(\d+)(\w+)$/) ?? [];
      if (unit === 'cm') return parseInt(num) >= 150 && parseInt(num) <= 193;
      if (unit === 'in') return parseInt(num) >= 59 && parseInt(num) <= 76;
      return false;
    },
    'hcl': (value) => !!value.match(/^\#([0-9a-f]{6})$/),
    'ecl': (value) => ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'].includes(value),
    'pid': (value) => !!value.match(/^(\d{9})$/),
  };
  const passports = parsePassports(lines);

  const validPassports = passports.filter(p => {
    const keys = Object.keys(p);
    const requiredKeys = Object.keys(requiredFields)
    for (const field of requiredKeys) {
      if (!keys.includes(field)) return false;
      if (!requiredFields[field](p[field])) return false;
    }
    return true;
  })
  return validPassports.length;
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day4 part1:', part1(input));
console.log('day4 part2:', part2(input));

/*
const input = require('../input');
const log = console.log;
const compose = (...fns) => (args) => fns.reduceRight((arg, fn) => fn(arg), args);
const split = (on) => (value) => value.split(on);

const splitOnBlankLine = split('\n\n');
const splitOnFieldSeparator = split(':');
const splitOnNewLine = split(/\s+/);

const parseToRecord = (passport) =>
  passport.reduce((record, field) => {
    const [key, value] = splitOnFieldSeparator(field);
    return { ...record, [key]: value };
  }, {});

const isWithinRange = (min, max) => (number) => number >= min && number <= max;
const isValidHeight = ([_, height, unit]) => (unit === 'cm' ? isWithinRange(150, 193) : isWithinRange(59, 76))(height);
const match = (pattern) => (string) => string.match(pattern) ?? [];
const matches = (pattern) => (string) => pattern.test(string);
const isAlwaysValid = () => true;

const validator = {
  byr: isWithinRange(1920, 2002),
  iyr: isWithinRange(2010, 2020),
  eyr: isWithinRange(2020, 2030),
  hgt: compose(isValidHeight, match(/^(\d+)(cm|in)$/)),
  hcl: matches(/^#[0-9a-f]{6}$/),
  ecl: matches(/^(amb|blu|brn|gry|grn|hzl|oth)$/),
  pid: matches(/^\d{9}$/),
  cid: isAlwaysValid,
};

const hasAllRequiredFields = (passport) =>
  ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'].every((field) => passport[field]);
const hasAllValidFields = (passport) => Object.entries(passport).every(([key, value]) => validator[key](value));

const passports = splitOnBlankLine(input(__dirname, './input.txt')).map(splitOnNewLine).map(parseToRecord);

const solutionOne = passports.filter(hasAllRequiredFields).length;
log(`Solution pt.1 ${solutionOne}`);

const solutionTwo = passports.filter(hasAllRequiredFields).filter(hasAllValidFields).length;
log(`Solution pt.2 ${solutionTwo}`);
*/