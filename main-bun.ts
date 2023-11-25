import fs from 'fs';

let proc;
process.on('SIGINT', () => {
  console.log('Ctrl-C was pressed');
  // if (proc) proc.kill(); // not needed
  // process.exit(); // not needed
});

let [, , year, day] = Bun.argv;
if (!day) {
  day = year;
  year = new Date().getFullYear().toString();
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

console.log(`Running ${year} ${day}`);

console.time('⬅️ Finished in');

let filename = `day${day}.ts`;
let file = Bun.file(`${path}/${filename}`);
if (!(await file.exists())) {
  filename = `day${day}.js`;
}

proc = Bun.spawn(['bun', filename], {
  cwd: path,
  stdin: null,
  stdout: 'inherit',
  stderr: 'inherit',
  onExit(proc, exitCode, signalCode, error) {
    if (exitCode !== 0) {
      console.log('Proc killed:', proc.killed);
      console.log('Exit code:', exitCode);
      console.log('Signal code:', signalCode);
      console.log('Error:', error);
    }
    console.timeEnd('⬅️ Finished in');
  },
});
