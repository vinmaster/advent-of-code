import fs from 'fs';

interface Options {
  year: number;
  day: number;
  overwrite: boolean;
}

const CONFIG = await Bun.file('config.json').json();

const args = convertArgs(Bun.argv);
const options = parseOptions(args);

// console.log(`Puzzle year is ${options.year}`);
// console.log(`Puzzle day is ${options.day}`);

const { year, day } = options;

const commands = args['_'];
if (commands.length === 0) {
  console.error('No commands given');
  printHelp();
  process.exit(0);
}

switch (commands[0]) {
  case 'input': {
    await downloadInput(year, day, options);
    break;
  }
  case 'copy':
    copyFile(year, day, commands[1], options);
    break;
  case 'submit': {
    await submit(year, day, commands[1], commands[2]);
    break;
  }
  case 'prompt': {
    await downloadPrompt(year, day, options);
    await cleanPrompt(year, day);
    break;
  }
  case 'list': {
    const ext = commands[1];
    await listFiles(year, ext);
    break;
  }
  case 'help':
    printHelp();
    break;
  default:
    throw new Error(`Invalid command: ${commands[0]}`);
}

// ----------------- FUNCTIONS -----------------

function printHelp() {
  console.log(`
Usage: bun cli.ts [OPTIONS] [COMMAND]

Commands:
  input                   Download puzzle input for the given day
  copy <ext>              Copies file with given extension from previous day
  submit <part> <answer>  Submit puzzle answer
  prompt                  Download prompt
  list <ext>              List solutions for the year. Can filter by extension
  help                    Print this message

Options:
  -d, --day <DAY>         Puzzle day [default: current day if in December]
  -y, --year <YEAR>       Puzzle year [default: current year]
  -o, --overwrite yes     Overwrite file if they already exist
`);
}

async function downloadInput(year: number, day: number, options?: Options) {
  let filePath = `${getPuzzlePath(year, day)}/input.txt`;
  if (!options?.overwrite && (await Bun.file(filePath).exists())) {
    console.log(`Input already exists: ${filePath} (Use to overwrite option (-o) to overwrite)`);
    return;
  }
  let url = `https://adventofcode.com/${year}/day/${day}/input`;
  let headers = { cookie: `session=${CONFIG.SESSION_ID}` };
  if (CONFIG.USER_AGENT) {
    headers['user-agent'] = CONFIG.USER_AGENT;
  }
  let response = await fetch(url, { headers });
  maybeCreateDirectory(year, day);
  // let text = (await response.text()) as string;
  await Bun.write(filePath, response);
  console.log(`File downloaded to: ${filePath}`);
}

function copyFile(year: number, day: number, ext: string, options: Options) {
  if (!ext) {
    console.error('No ext given');
    return;
  }
  maybeCreateDirectory(year, day);
  const puzzlePath = getPuzzlePath(year, day);
  const currentPath = `${puzzlePath}/day${day}.${ext}`;
  let previousDay = day - 1;
  let isComplete = false;
  const copyFromYear = (year: number) => {
    while (previousDay >= 1) {
      const file = `day${previousDay}.${ext}`;
      const previousPath = `./${year}/day${previousDay}/${file}`;
      if (!options.overwrite && fs.existsSync(currentPath)) {
        console.log(
          `File already exists: ${currentPath} (Use to overwrite option (-o) to overwrite)`
        );
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
  };
  copyFromYear(year);
  // Look in previous year
  if (previousDay === 0) {
    previousDay = 25;
    copyFromYear(year - 1);
  }
  if (!isComplete) {
    console.log('No previous file to copy this year');
  }
}

async function submit(year, day, part, answer) {
  if (!part || !answer) {
    console.error('No part or answer given');
    return;
  }
  let url = `https://adventofcode.com/${year}/day/${day}/answer`;
  let body = `level=${part}&answer=${answer}`;
  let headers = {
    cookie: `session=${CONFIG.SESSION_ID}`,
    'Content-Type': 'application/x-www-form-urlencoded',
    'Content-Length': Buffer.byteLength(body),
  };
  if (CONFIG.USER_AGENT) {
    headers['user-agent'] = CONFIG.USER_AGENT;
  }
  const response = await fetch(url, {
    method: 'POST',
    body,
    headers,
  });

  const result = await response.text();
  try {
    let [, matches] = [
      ...result.replaceAll('\n', '').matchAll(/.*\<article\>(.*)\<\/article\>/g),
    ][0];
    console.log(matches);
    return matches;
  } catch (error) {
    console.error(error);
    throw error;
  }
}

async function downloadPrompt(year, day, options) {
  maybeCreateDirectory(year, day);
  const puzzlePath = getPuzzlePath(year, day);
  let path = `${puzzlePath}/README.md`;
  if (!options.overwrite && fs.existsSync(path)) {
    console.log(`Prompt already exists: ${path} (Use to overwrite option (-o) to overwrite)`);
    return;
  }
  let url = `https://adventofcode.com/${year}/day/${day}`;
  let headers = { cookie: `session=${CONFIG.SESSION_ID}` };
  if (CONFIG.USER_AGENT) {
    headers['user-agent'] = CONFIG.USER_AGENT;
  }
  let response = await fetch(url, { headers });
  await Bun.write(path, response);
  console.log(`Prompt downloaded to: ${path}`);
}

async function listFiles(year: number, ext?: string) {
  if (year) {
    let yearPath = `./${year}`;
    if (fs.existsSync(yearPath)) {
      let days = (await fs.promises.readdir(yearPath)) as string[];
      days = days
        .map(d => Number(d.replace('day', '')))
        .sort((a, b) => a - b)
        .map(n => `day${n}`);
      for (const day of days) {
        let files = (await fs.promises.readdir(`${yearPath}/${day}`)) as string[];
        if (ext)
          console.log(
            day,
            files.filter(f => !f.endsWith('.md') && !f.endsWith('.txt') && f.endsWith(`.${ext}`))
          );
        else
          console.log(
            day,
            files.filter(f => !f.endsWith('.md') && !f.endsWith('.txt'))
          );
      }
    }
  }
}

function cleanPrompt(year, day) {
  maybeCreateDirectory(year, day);
  const puzzlePath = getPuzzlePath(year, day);
  let path = `${puzzlePath}/README.md`;
  if (!fs.existsSync(path)) {
    console.error(`Prompt does not exist: ${path}`);
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

function maybeCreateDirectory(year: number, day: number) {
  const yearDirectory = `./${year}`;
  const puzzlePath = getPuzzlePath(year, day);
  if (!fs.existsSync(yearDirectory)) {
    fs.mkdirSync(yearDirectory);
  }
  if (!fs.existsSync(puzzlePath)) {
    fs.mkdirSync(puzzlePath);
  }
}

function convertArgs(arglist: any[]) {
  arglist = arglist.slice(2);
  const args: Record<string, any> = { _: [] as any[] };
  for (let i = 0; i < arglist.length; i++) {
    if (arglist[i].includes('-')) {
      args[arglist[i]] = arglist[i + 1];
      i++;
    } else {
      args['_'].push(arglist[i]);
    }
  }
  return args;
}

function parseOptions(args: Record<string, any>): Options {
  let today = new Date();
  // Provide defaults
  let options = {
    year: today.getFullYear(),
    day: today.getDate(),
    overwrite: false,
  };
  // Puzzle day come out at 5am UTC
  if (today.getUTCHours() >= 5) {
    options.day += 1;
  }
  // Puzzles comes out in December (month 11). Try to calculate current year
  if (today.getUTCMonth() !== 11) {
    options.year -= 1;
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

function getPuzzlePath(year: number, day: number) {
  return `./${year}/day${day}`;
}
