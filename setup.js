const { parse } = require('url');
const http = require('https');
const fs = require('fs');
const { basename } = require('path');
const CONFIG = require('./config.json');

const TIMEOUT = 10000;

(async () => {
  /** @type string[] */
  let arglist = process.argv.slice(2, process.argv.length);
  let args = { _: [] };

  for (let i = 0; i < arglist.length; i++) {
    if (arglist[i].includes('-')) {
      args[arglist[i]] = arglist[i + 1];
      i++;
    } else {
      args['_'].push(arglist[i]);
    }
  }

  // Provide year and day
  let [year, day] = args['_'].map(x => parseInt(x, 10));

  if (day === undefined) {
    // Only day was provided
    day = year;
    year = new Date().getFullYear();
  }

  if (!year || !day) {
    throw new Error('No year and day');
  }

  const yearDirectory = `./${year}`;
  if (!fs.existsSync(yearDirectory)) {
    fs.mkdirSync(yearDirectory);
  }

  const directory = `./${year}/day${day}`;
  if (!fs.existsSync(directory)) {
    fs.mkdirSync(directory);
  }

  // Parse flags
  for (let key of Object.keys(args)) {
    if (key === '_') continue;

    switch (key) {
      case '-f':
      case '--file':
        let previousDay = day - 1;
        while (previousDay >= 1) {
          let file = `day${previousDay}.${args[key]}`;
          let previousPath = `./${year}/day${previousDay}/${file}`;
          let currentPath = `${directory}/day${day}.${args[key]}`;
          if (fs.existsSync(currentPath)) {
            console.log(`File already exists: ${currentPath}`);
            break;
          }
          if (fs.existsSync(previousPath)) {
            fs.copyFileSync(previousPath, currentPath);
            console.log(`Copied file ${previousPath} to ${currentPath}`);
            break;
          }
          previousDay--;
        }
        break;
      case '-h':
      case '--help':
        console.log(`node setup.js [YEAR] DAY

Options:

  -f --file: Copies file extension from previous day
`);
        break;
      default:
        throw new Error(`Unknown flag: ${key}`);
        break;
    }
  }

  return;

  try {
    await download(`https://adventofcode.com/${year}/day/${day}/input`, `${directory}/input.txt`);
  } catch (error) {
    console.error(error.message);
  }
})();

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
