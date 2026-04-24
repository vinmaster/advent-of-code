import fs from 'fs';

let proc;
process.on('SIGINT', () => {
  console.log('Ctrl-C was pressed');
  if (proc) proc.kill(); // TODO check if needed
  process.exit(); // TODO check if needed
});

let [, , year, day] = Bun.argv;
const today = new Date();
if (day === undefined) {
  day = year;
  year = today.getFullYear();
} else {
  year = year ?? today.getFullYear();
  day = day ?? today.getDate();
}

// Puzzles come out in December (month 11).
if (today.getUTCMonth() !== 11) {
  // If not December, default to last year's puzzles
  year -= 1;
} else {
  // If it is December, check time
  // Puzzle day come out at 5am UTC (midnight EST)
  if (today.getUTCHours() < 5) {
    day -= 1; // It's still the previous puzzle day
  }
}

let path = `${year}/day${day}`;

if (day === undefined) {
  console.error(`Input error: year=${year} and day=${day}`);
  process.exit(1);
}

// let file = Bun.file(path);
// if (!(await file.exists())) {
if (!fs.existsSync(path)) {
  console.error(`Input error: path doesn't exist '${path}'`);
  process.exit(1);
}

let filename = `day${day}.ts`;
let file = Bun.file(`${path}/${filename}`);
if (!(await file.exists())) {
  filename = `day${day}.js`;
}

console.time('⬅️ Finished in');

let delay = 10000;
let timeoutId = setTimeout(() => {
  if (proc) proc.kill(); // TODO check if needed
  console.log(`Proc ran over ${delay}ms`);
}, delay);

proc = Bun.spawn(['bun', filename], {
  cwd: path,
  stdin: null,
  stdout: 'inherit',
  stderr: 'inherit',
  onExit(proc, exitCode, signalCode, error) {
    if (exitCode !== 0) {
      console.log('Proc killed:', proc.killed);
      // console.log('Exit code:', exitCode);
      // console.log('Signal code:', signalCode);
      // console.log('Error:', error);
    } else {
      console.timeEnd('⬅️ Finished in');
    }
    clearTimeout(timeoutId);
  },
});
