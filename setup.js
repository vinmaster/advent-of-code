const { parse } = require('url');
const http = require('https');
const fs = require('fs');
const { basename } = require('path');
const CONFIG = require('./config.json');

const TIMEOUT = 10000;

(async () => {
  // Provide year and day
  let year = parseInt(process.argv[2], 10);
  let day = parseInt(process.argv[3], 10);

  if (process.argv.length === 3) {
    // Only day was provided
    day = year;
    year = (new Date()).getFullYear();
  }

  if (!year || !day) { throw new Error('No year and day'); }

  const yearDirectory = `./${year}`;
  if (!fs.existsSync(yearDirectory)) {
    fs.mkdirSync(yearDirectory);
  }

  const directory = `./${year}/day${day}`;
  if (!fs.existsSync(directory)) {
    fs.mkdirSync(directory);
  }

  await download(`https://adventofcode.com/${year}/day/${day}/input`, `${directory}/input.txt`);
})();

function download(url, path) {
  const uri = parse(url);
  if (!path) {
    path = basename(uri.path);
  }
  const file = fs.createWriteStream(path);

  return new Promise(function(resolve, reject) {
    const options = {
      headers: {
        cookie: `session=${CONFIG.SESSION_ID}`,
      },
    };
    const request = http.get(uri.href, options).on('response', function(res) {
      const len = parseInt(res.headers['content-length'], 10);
      let downloaded = 0;
      let percent = 0;
      res
        .on('data', function(chunk) {
          file.write(chunk);
          downloaded += chunk.length;
          percent = (100.0 * downloaded / len).toFixed(2);
          process.stdout.write(`Downloading ${percent}% ${downloaded} bytes\r`);
        })
        .on('end', function() {
          file.end();
          console.log(`${uri.path} downloaded to: ${path}`);
          resolve();
        })
        .on('error', function (err) {
          reject(err);
        })
    })
    request.setTimeout(TIMEOUT, function() {
      request.abort();
      reject(new Error(`request timeout after ${TIMEOUT / 1000.0}s`));
    })
  });
}
