const { parse } = require('url');
const http = require('https');
const fs = require('fs');
const { basename } = require('path');
const CONFIG = require('./config.json');

const TIMEOUT = 10000;

(async () => {
  /** @type string[] */
  const arglist = process.argv.slice(2, process.argv.length);

  const args = { _: [] };
  for (let i = 0; i < arglist.length; i++) {
    if (arglist[i].includes('-')) {
      args[arglist[i]] = arglist[i + 1];
      i++;
    } else {
      args['_'].push(arglist[i]);
    }
  }

  // Parse options
  const options = parseOptions(args);
  const { year, day } = options;

  const commands = args['_'];
  if (commands.length === 0) {
    console.log('No commands given');
    printHelp();
    return;
  }

  switch (commands[0]) {
    case 'download':
      await downloadInput(year, day, options);
      break;
    case 'copy':
      copyFile(year, day, commands[1], options);
      break;
    case 'submit':
      console.log('Unimplemented');
      break;
    case 'help':
      printHelp();
      break;
    default:
      throw new Error(`Invalid command: ${key}`);
      break;
  }
})();

// ----------------- COMMANDS -----------------
async function downloadInput(year, day, options) {
  try {
    let inputPath = `${getDirectory(year, day)}/input.txt`;
    if (!options.overwrite && fs.existsSync(inputPath)) {
      console.log(`Input already exists: ${inputPath} (Use to overwrite option to overwrite)`);
      return;
    }
    await download(
      `https://adventofcode.com/${year}/day/${day}/input`,
      `${getDirectory(year, day)}/input.txt`
    );
  } catch (error) {
    console.error(error.message);
  }
}

function copyFile(year, day, ext, options) {
  if (!ext) {
    console.log('No ext given');
    return;
  }
  const directory = getDirectory(year, day);
  let previousDay = day - 1;
  let isComplete = false;
  while (previousDay >= 1) {
    const file = `day${previousDay}.${ext}`;
    const previousPath = `./${year}/day${previousDay}/${file}`;
    const currentPath = `${directory}/day${day}.${ext}`;
    if (!options.overwrite && fs.existsSync(currentPath)) {
      console.log(`File already exists: ${currentPath} (Use to overwrite option to overwrite)`);
      isComplete = true;
      break;
    }
    if (fs.existsSync(previousPath)) {
      fs.copyFileSync(previousPath, currentPath);
      console.log(`Copied file ${previousPath} to ${currentPath}`);
      isComplete = true;
      break;
    }
    previousDay--;
  }
  if (!isComplete) {
    console.log('No previous file to copy this year');
  }
}

function printHelp() {
  console.log(`
Usage: node cli.js [OPTIONS] [COMMAND]

Commands:
  download    Download puzzle input for the given day
  copy <ext>  Copies file with given extension from previous day
  submit      Submit puzzle answer
  help        Print this message

Options:
  -d, --day <DAY>            Puzzle day [default: current day if in December]
  -y, --year <YEAR>          Puzzle year [default: current year]
  -o, --overwrite yes        Overwrite file if they already exist
`);
}

// ----------------- UTILS -----------------
function parseOptions(args) {
  let today = new Date();
  // Provide defaults
  let options = {
    year: today.getFullYear(),
    day: today.getDate(),
    overwrite: false,
  };
  // Puzzle day come out at 9pm PST
  if (today.getHours() >= 21) {
    day += 1;
    console.log(`Puzzle day is ${day}`);
  }

  for (const key of Object.keys(args)) {
    if (key === '_') continue;
    switch (key) {
      case '-d':
      case '--day':
        options.day = parseInt(args[key], 10);
        break;
      case '-y':
      case '--year':
        options.year = parseInt(args[key], 10);
        break;
      case '-o':
      case '--overwrite':
        options.overwrite = true;
        break;
      case '-h':
      case '--help':
        printHelp();
        break;
      default:
        throw new Error(`Invalid option: ${key}`);
        break;
    }
  }
  return options;
}

function getDirectory(year, day) {
  return `./${year}/day${day}`;
}

function download(url, path) {
  const uri = parse(url);
  if (!path) {
    path = basename(uri.path);
  }
  const file = fs.createWriteStream(path);

  return new Promise(function (resolve, reject) {
    const options = {
      headers: {
        cookie: `session=${CONFIG.SESSION_ID}`,
      },
    };
    const request = http.get(uri.href, options).on('response', function (res) {
      if (res.statusCode !== 200) {
        console.log(`Status code: ${res.statusCode}`);
        return reject(new Error('Error fetching input'));
      }
      const len = parseInt(res.headers['content-length'], 10);
      let downloaded = 0;
      let percent = 0;
      res
        .on('data', function (chunk) {
          file.write(chunk);
          downloaded += chunk.length;
          percent = ((100.0 * downloaded) / len).toFixed(2);
          process.stdout.write(`Downloading ${percent}% ${downloaded} bytes\r`);
        })
        .on('end', function () {
          file.end();
          console.log(`${uri.path} downloaded to: ${path}`);
          file.on('finish', () => {
            resolve();
          });
        })
        .on('error', function (err) {
          fs.unlink(path);
          reject(err);
        });
    });
    request.setTimeout(TIMEOUT, function () {
      fs.unlink(path);
      request.abort();
      reject(new Error(`request timeout after ${TIMEOUT / 1000.0}s`));
    });
  });
}
