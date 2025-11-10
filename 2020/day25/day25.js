const fs = require('fs');
const path = require('path');

function modPow(base, exp, mod) {
  // modular exponentiation by squaring
  let result = 1;
  let b = base % mod;
  let e = exp;
  while (e > 0) {
    if (e & 1) result = (result * b) % mod;
    b = (b * b) % mod;
    e >>= 1;
  }
  return result;
}

function part1(input) {
  const [cardPub, doorPub] = input.trim().split('\n');
  let cardLoop = 1000000;
  let modNum = 20201227;
  while (modPow(7, cardLoop, modNum) != cardPub) {
    cardLoop += 1;
  }
  let doorLoop = 1000000;
  while (modPow(7, doorLoop, modNum) != doorPub) {
    doorLoop += 1;
  }
  let encryptionKey = modPow(cardPub, doorLoop, modNum);
  // console.log(cardLoop, doorLoop);
  // console.log('other key', modPow(doorPub, cardLoop, modNum));

  return encryptionKey;
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

console.log('part1:', part1(input));
