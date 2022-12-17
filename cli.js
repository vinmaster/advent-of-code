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
    case 'download': {
      await downloadInput(year, day, options);
      break;
    }
    case 'copy':
      copyFile(year, day, commands[1], options);
      break;
    case 'submit': {
      let result = await submit(year, day, commands[1], commands[2]);
      if (result) console.log(`Submit ${year} ${day}: ${parseSubmit(result)}`);
      break;
    }
    case 'prompt': {
      await downloadPrompt(year, day, options);
      await cleanPrompt(year, day);
      break;
    }
    case 'help':
      printHelp();
      break;
    default:
      throw new Error(`Invalid command: ${key}`);
  }
})();

// ----------------- COMMANDS -----------------
function copyFile(year, day, ext, options) {
  if (!ext) {
    console.log('No ext given');
    return;
  }
  const directory = getDirectory(year, day);
  maybeCreateDirectory(year, day);
  const currentPath = `${directory}/day${day}.${ext}`;
  let previousDay = day - 1;
  let isComplete = false;
  while (previousDay >= 1) {
    const file = `day${previousDay}.${ext}`;
    const previousPath = `./${year}/day${previousDay}/${file}`;
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
  download                Download puzzle input for the given day
  copy <ext>              Copies file with given extension from previous day
  submit <part> <answer>  Submit puzzle answer
  prompt                  Get prompt
  help                    Print this message

Options:
  -d, --day <DAY>         Puzzle day [default: current day if in December]
  -y, --year <YEAR>       Puzzle year [default: current year]
  -o, --overwrite yes     Overwrite file if they already exist
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
    options.day += 1;
    console.log(`Puzzle day is ${options.day}`);
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

function maybeCreateDirectory(year, day) {
  const yearDirectory = `./${year}`;
  if (!fs.existsSync(yearDirectory)) {
    fs.mkdirSync(yearDirectory);
  }
  const directory = getDirectory(year, day);
  if (!fs.existsSync(directory)) {
    fs.mkdirSync(directory);
  }
}

function downloadInput(year, day, options) {
  const directory = getDirectory(year, day);
  maybeCreateDirectory(year, day);
  let path = `${directory}/input.txt`;
  if (!options.overwrite && fs.existsSync(path)) {
    console.log(`Input already exists: ${path} (Use to overwrite option to overwrite)`);
    return;
  }
  let url = `https://adventofcode.com/${year}/day/${day}/input`;

  const uri = parse(url);
  if (!path) {
    path = basename(uri.path);
  }
  const file = fs.createWriteStream(path);

  return new Promise(function (resolve, reject) {
    const httpOptions = {
      headers: {
        cookie: `session=${CONFIG.SESSION_ID}`,
      },
    };
    if (CONFIG.USER_AGENT) {
      httpOptions.headers['user-agent'] = CONFIG.USER_AGENT;
    }
    const request = http.get(uri.href, httpOptions).on('response', function (res) {
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
          file.on('finish', () => {
            console.log(`${year} ${day} downloaded to: ${path}`);
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
      request.destroy();
      reject(new Error(`request timeout after ${TIMEOUT / 1000.0}s`));
    });
  });
}

function downloadPrompt(year, day, options) {
  const directory = getDirectory(year, day);
  maybeCreateDirectory(year, day);
  let path = `${directory}/README.md`;
  if (!options.overwrite && fs.existsSync(path)) {
    console.log(`Prompt already exists: ${path} (Use to overwrite option to overwrite)`);
    return;
  }
  let url = `https://adventofcode.com/${year}/day/${day}`;

  const uri = parse(url);
  if (!path) {
    path = basename(uri.path);
  }
  const file = fs.createWriteStream(path);

  return new Promise(function (resolve, reject) {
    const httpOptions = {
      headers: {
        cookie: `session=${CONFIG.SESSION_ID}`,
      },
    };
    if (CONFIG.USER_AGENT) {
      httpOptions.headers['user-agent'] = CONFIG.USER_AGENT;
    }
    const request = http.get(uri.href, httpOptions).on('response', function (res) {
      if (res.statusCode !== 200) {
        console.log(`Status code: ${res.statusCode}`);
        return reject(new Error('Error fetching prompt'));
      }
      res.on('error', function (err) {
        fs.unlink(path);
        reject(err);
      });
      file.on('finish', () => {
        console.log(`${year} ${day} downloaded to: ${path}`);
        resolve({});
      });

      res.pipe(file);
    });
    request.setTimeout(TIMEOUT, function () {
      fs.unlink(path);
      request.destroy();
      reject(new Error(`request timeout after ${TIMEOUT / 1000.0}s`));
    });
  });
}

function cleanPrompt(year, day) {
  const directory = getDirectory(year, day);
  maybeCreateDirectory(year, day);
  let path = `${directory}/README.md`;
  if (!fs.existsSync(path)) {
    console.log(`Prompt does not exist: ${path}`);
    return;
  }
  let file = fs.readFileSync(path, 'utf8');
  let lines = file.split('\n');
  let isMain = false;
  for (let i = 0; i < lines.length; ) {
    if (lines[i].includes('main>')) isMain = !isMain;
    if (!isMain) lines.splice(i, 1);
    else i++;
  }
  lines.push('</main>', '');
  fs.writeFileSync(path, lines.join('\n'));
}

function submit(year, day, part, answer) {
  if (!part || !answer) {
    console.log('No part or answer given');
    return;
  }
  return new Promise(function (resolve, reject) {
    let body = `level=${part}&answer=${answer}`;
    const options = {
      host: 'adventofcode.com',
      path: `/${year}/day/${day}/answer`,
      headers: {
        accept: 'text/html',
        'content-type': 'application/x-www-form-urlencoded',
        'content-length': Buffer.byteLength(body),
        cookie: `session=${CONFIG.SESSION_ID}`,
      },
      method: 'POST',
    };
    if (CONFIG.USER_AGENT) {
      options.headers['user-agent'] = CONFIG.USER_AGENT;
    }

    let request = http.request(options, res => {
      res.setEncoding('utf8');
      if (res.statusCode !== 200) {
        console.log(`Status code: ${res.statusCode}`);
        return reject(new Error('Error submitting'));
      }
      let str = '';
      res
        .on('data', function (chunk) {
          str += chunk;
        })
        .on('end', function () {
          resolve(str);
        })
        .on('error', function (err) {
          reject(err);
        });
    });
    request.write(body);
    request.end();
  });
}

function parseSubmit(result) {
  try {
    let [, matches] = [
      ...result.replaceAll('\n', '').matchAll(/.*\<article\>(.*)\<\/article\>/g),
    ][0];
    return matches;
  } catch (error) {
    console.log(result);
    throw error;
  }
}
