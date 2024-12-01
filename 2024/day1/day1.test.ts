const testInput = `
3   4
4   3
2   5
1   3
3   9
3   3
`;

// Deno
// import { assertEquals } from 'jsr:@std/assert';
// import { part1, part2 } from './day1.ts';

// Deno.test('2024 day1 part1', () => {
//   assertEquals(part1(testInput), 11);
// });

// Deno.test('2024 day1 part2', () => {
//   assertEquals(part2(testInput), 31);
// });

// Bun
import { expect, test } from 'bun:test';
import { part1, part2 } from './day1.ts';

test('2024 day1 part1', () => {
  expect(part1(testInput)).toBe(11);
});

test('2024 day1 part2', () => {
  expect(part2(testInput)).toBe(31);
});
