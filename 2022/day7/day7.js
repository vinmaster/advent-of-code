const fs = require('fs');
const path = require('path');

function getDirSize(tree, dir, path = '', sum = 0) {
  for (let [key, value] of Object.entries(tree)) {
    if (typeof value === 'object') {
      let newPath = `${path}/${key}`;
      let [done, s] = getDirSize(value, dir, newPath);
      if (done) return [done, s];
      sum += s;
      if (newPath === dir) return [true, s];
    } else {
      sum += parseInt(value, 10);
    }
  }
  return [false, sum];
}

function getAllDirs(tree, path = '') {
  let dirs = [];
  for (let [key, value] of Object.entries(tree)) {
    if (typeof value === 'object') {
      let newPath = `${path}/${key}`;
      dirs = [...dirs, newPath, ...getAllDirs(value, newPath)];
    }
  }
  return dirs;
}

function getFs(lines) {
  let fsStack = [{}];
  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];

    if (line.includes('$')) {
      let [, cmd, arg] = [...line.matchAll(/\$ (cd|ls)\s?(.+)?/g)].at(0);
      if (cmd === 'cd' && arg === '/') {
        continue;
      } else if (cmd === 'cd' && arg === '..') {
        fsStack.pop();
      } else if (cmd === 'cd') {
        fsStack.push(fsStack.at(-1)[arg.trim()]);
      } else if (cmd === 'ls') {
        do {
          i++;
          line = lines[i];
          let [, sizeOrDir, name] = [...line.matchAll(/(.+)\s(.+)/g)].at(0);
          if (sizeOrDir === 'dir') {
            fsStack.at(-1)[name] = {};
          } else {
            fsStack.at(-1)[name] = sizeOrDir;
          }
        } while (i + 1 < lines.length && !lines[i + 1].includes('$'));
        i--;
      }
    }
  }
  return fsStack.at(0);
}

function part1(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let fs = getFs(lines);
  // console.log(JSON.stringify(fs, null, 2));

  let dirs = getAllDirs(fs);
  // dirs.forEach(d => console.log(d));

  let dirSizes = dirs.map(d => [d, getDirSize(fs, d)[1]]).filter(s => s[1] < 100000);
  dirSizes.sort((a, b) => a[1] - b[1]);
  // console.log(dirSizes);

  return dirSizes
    .map(s => s[1])
    .filter(s => s < 100000)
    .reduce((a, b) => a + b, 0);
}

function part2(input) {
  /** @type string[] */
  let lines = input.trim().split('\n');
  let fs = getFs(lines);
  // console.log(JSON.stringify(fs, null, 2));

  let dirs = getAllDirs(fs);
  // dirs.forEach(d => console.log(d));

  let dirSizes = dirs.map(d => [d, getDirSize(fs, d)[1]]);
  dirSizes.sort((a, b) => a[1] - b[1]);
  // console.log(dirSizes);

  let fsMax = 70_000_000;
  let updateSize = 30_000_000;
  let fsSize = getDirSize(fs, '/')[1];
  for (let [_, size] of dirSizes) {
    if (fsSize - size + updateSize < fsMax) {
      return size;
    }
  }
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8').replaceAll('\r', '');

// input = `
// $ cd /
// $ ls
// dir a
// 14848514 b.txt
// 8504156 c.dat
// dir d
// $ cd a
// $ ls
// dir e
// 29116 f
// 2557 g
// 62596 h.lst
// $ cd e
// $ ls
// 584 i
// $ cd ..
// $ cd ..
// $ cd d
// $ ls
// 4060174 j
// 8033020 d.log
// 5626152 d.ext
// 7214296 k`;

console.log('part1:', part1(input));
console.log('part2:', part2(input));
