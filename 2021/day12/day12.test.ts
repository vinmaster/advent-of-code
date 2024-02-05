// @ts-ignore: next-line
import { expect, test } from 'bun:test';
import { part1, part2 } from './day12';

const TEST_INPUT_1 = `
start-A
start-b
A-c
A-b
b-d
A-end
b-end`;

const TEST_INPUT_2 = `
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc`;

const TEST_INPUT_3 = `
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW`;

test('true', () => {
  expect(true).toBe(true);
});

test('part1 test input 1', () => {
  expect(part1(TEST_INPUT_1)).toBe(10);
});

test('part1 test input 2', () => {
  expect(part1(TEST_INPUT_2)).toBe(19);
});

test('part1 test input 3', () => {
  expect(part1(TEST_INPUT_3)).toBe(226);
});

test('part2 test input 1', () => {
  expect(part2(TEST_INPUT_1)).toBe(36);
});

test('part2 test input 2', () => {
  expect(part2(TEST_INPUT_2)).toBe(103);
});

test('part2 test input 3', () => {
  expect(part2(TEST_INPUT_3)).toBe(3509);
});
