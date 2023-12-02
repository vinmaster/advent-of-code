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
  case 'download': {
    await downloadInput(year, day, options);
    break;
  }
  case 'copy':
    copyFile(year, day, commands[1], options);
    break;
  case 'submit': {
    // let result = await submit(year, day, commands[1], commands[2]);
    // if (result) console.log(`Submit ${year} ${day}: ${parseSubmit(result)}`);
    break;
  }
  case 'prompt': {
    // await downloadPrompt(year, day, options);
    // await cleanPrompt(year, day);
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
  download                Download puzzle input for the given day
  copy <ext>              Copies file with given extension from previous day
  submit <part> <answer>  Submit puzzle answer
  prompt                  Get prompt
  list <ext>              List solutions for the year. Can filter by extension
  help                    Print this message

Options:
  -d, --day <DAY>         Puzzle day [default: current day if in December]
  -y, --year <YEAR>       Puzzle year [default: current year]
  -o, --overwrite yes     Overwrite file if they already exist
`);
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

function getPuzzlePath(year: number, day: number) {
  return `./${year}/day${day}`;
}

async function downloadInput(year: number, day: number, options?: Options) {
  let url = `https://adventofcode.com/${year}/day/${day}/input`;
  let response = await fetch(url, {
    headers: { cookie: `session=${CONFIG.SESSION_ID}` },
  });
  maybeCreateDirectory(year, day);
  let filePath = `${getPuzzlePath(year, day)}/input.txt`;
  // let text = (await response.text()) as string;
  await Bun.write(filePath, response);
}

function copyFile(year: number, day: number, ext: string, options: Options) {
  if (!ext) {
    console.log('No ext given');
    return;
  }
  maybeCreateDirectory(year, day);
  const puzzlePath = getPuzzlePath(year, day);
  const currentPath = `${puzzlePath}/day${day}.${ext}`;
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
  // Puzzle day come out at 9pm PST
  if (today.getHours() >= 21) {
    options.day += 1;
  }
  // Puzzles comes out in December (month 11)
  if (today.getMonth() < 11) {
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
