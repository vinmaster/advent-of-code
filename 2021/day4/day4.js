const fs = require('fs');
const path = require('path');

function range(n) {
  return [...Array(n).keys()];
}

function transpose(matrix) {
  return matrix.reduce((prev, next) => next.map((item, i) =>
    (prev[i] || []).concat(next[i])
  ), []);
}

/** @param {string} line */
function parseBoardLine(line) {
  return line.split(' ').filter(s => s.length > 0).map(s => parseInt(s, 10));
}

/** @param {string} input */
function parseInput(input) {
  let lines = input.trim().split('\n');
  let numsStr = lines.shift();
  lines.shift();
  let nums = numsStr.split(',').map(s => parseInt(s, 10));
  let boards = [];

  while (lines.length > 0) {
    let board = [];
    for (let i = 0; i < 5; i++) {
      let line = lines.shift();
      board.push(parseBoardLine(line));
    }
    lines.shift();
    boards.push(board);
  }

  return { nums, boards }
}

/** 
 * @param {number[][]} board 
 * @param {number} num
*/
function markBoard(board, num) {
  for (let row of board) {
    let index = row.indexOf(num);
    if (index !== -1) {
      row[index] = `x${row[index]}`;
    }
  }
}

/** 
 * @param {number[][]} board 
 * @returns {boolean}
*/
function isBingo(board) {
  let hasX = x => x.toString().includes('x');
  for (let row of board) {
    if (row.every(hasX)) return true;
  }
  // for (let col of transpose(board)) {
  //   if (col.every(hasX)) return true;
  // }
  for (let i = 0; i < board[0].length; i++) {
    if (board.every(row => hasX(row[i]))) return true;
  }
  return false;
}

/** 
 * @param {number[][]} board 
 * @returns {number}
*/
function getSum(board) {
  let sum = 0;
  for (let row of board) {
    sum = row.reduce((acc, cur) => {
      if (!cur.toString().includes('x')) {
        return acc + cur;
      }
      return acc;
    }, sum)
  }
  return sum;
}

const part1 = input => {
  let { nums, boards } = parseInput(input);

  for (let i = 0; i < nums.length; i++) {
    const num = nums[i];
    for (let board of boards) {
      markBoard(board, num);
      if (isBingo(board)) {
        let sum = getSum(board);
        return sum * num;
      }
    }
  }
};

const part2 = input => {
  let { nums, boards } = parseInput(input);
  let wonBoards = range(boards.length);
  let lastNum = -1;
  let done = false;

  for (let i = 0; i < nums.length; i++) {
    lastNum = nums[i];
    for (let j = 0; j < boards.length; j++) {
      let board = boards[j];
      markBoard(board, lastNum);
      if (isBingo(board)) {
        if (wonBoards.length === 1 && j === wonBoards[0]) {
          done = true;
          break;
        }
        if (wonBoards.includes(j)) wonBoards.splice(wonBoards.indexOf(j), 1);
      }
    }
    if (done) break;
  }

  let sum = getSum(boards[wonBoards[0]]);
  return sum * lastNum;
};

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');

// input = `7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

// 22 13 17 11  0
//  8  2 23  4 24
// 21  9 14 16  7
//  6 10  3 18  5
//  1 12 20 15 19

//  3 15  0  2 22
//  9 18 13 17  5
// 19  8  7 25 23
// 20 11 10 24  4
// 14 21 16 12  6

// 14 21 17 24  4
// 10 16 15  9 19
// 18  8 23 26 20
// 22 11 13  6  5
//  2  0 12  3  7`

console.log('day4 part1:', part1(input));
console.log('day4 part2:', part2(input));
