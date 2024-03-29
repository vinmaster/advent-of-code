const fs = require('fs');
const util = require('util');
const { exec } = require('child_process');
const execPromise = util.promisify(exec);

// for (let j = 0; j < process.argv.length; j++) {
//   console.log(j + ' -> ' + (process.argv[j]));
// }

const run = async (command) => {
  try {
    const { stdout, stderr } = await execPromise(command)
    return stdout;
  } catch (error) {
    console.log(error.stdout);
    console.log('--- Error --------');
    console.log(error.stderr);
    console.log('------------------');
    // console.error(error);
    return null;
  }
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
      if (output !== null) console.log(output.trim());
    }
  } else {
    if (process.argv.length === 3) {
      // Only day was provided
      day = year;
      year = (new Date()).getFullYear();
    }
    console.log(`Running year ${year} day ${day}`);
    const hrStartTime = process.hrtime();
    const output = await run(`node ./${year}/day${day}/day${day}.js`);
    const hrEndTime = process.hrtime(hrStartTime);
    const MS_PER_NS = 1000000;
    console.log('Finished in:', `${hrEndTime[0]}.${Math.round(hrEndTime[1] / MS_PER_NS)} s`);
    if (output !== null) console.log(output.trim());
  }
})();
