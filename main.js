const fs = require('fs');
const util = require('util');
const { exec } = require('child_process');
const execPromise = util.promisify(exec);

// for (let j = 0; j < process.argv.length; j++) {
//   console.log(j + ' -> ' + (process.argv[j]));
// }

const run = async (command) => {
  const { stdout, stderr } = await execPromise(command)
  return stdout;
}

(async () => {
  // Provide year and day
  let year = parseInt(process.argv[2], 10);
  let day = parseInt(process.argv[3], 10);

  if (process.argv.length === 3 && process.argv[2].length > 2) {
    // Only year was provided
    console.log(`Running year ${year}`);
    let days = await run(`ls ./${year}`);
    days = days.trim().split('\n');
    for (const day of days) {
      if (!fs.existsSync(`./${year}/${day}/${day}.js`)) continue;

      const output = await run(`node ./${year}/${day}/${day}.js`);
      console.log(output.trim());
    }
  } else {
    if (process.argv.length === 3) {
      // Only day was provided
      day = year;
      year = (new Date()).getFullYear();
    }
    console.log(`Running year ${year} and day ${day}`);
    const output = await run(`node ./${year}/day${day}/day${day}.js`);
    console.log(output.trim());
  }
})();
