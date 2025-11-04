import fs from 'fs';
import path from 'path';
import type { Subprocess } from 'bun';

// -----------------------------------------------------------------
// ## 1. Global Setup
// -----------------------------------------------------------------

// Global process handler for killing the spawned solution
let proc: Subprocess | undefined;
process.on('SIGINT', () => {
  console.log('\nCtrl-C was pressed');
  if (proc) proc.kill();
  process.exit();
});

// --- Interfaces ---
interface Config {
  SESSION_ID: string;
  USER_AGENT?: string;
}

interface Options {
  year: number;
  day: number;
  overwrite: boolean;
  help: boolean;
}

interface ParsedArgs {
  command: string;
  options: Options;
  args: string[];
}

// -----------------------------------------------------------------
// ## 2. Configuration & Utilities
// -----------------------------------------------------------------

/**
 * Loads and validates the config.json file.
 */
async function loadConfig(): Promise<Config> {
  const configPath = 'config.json';
  if (!(await Bun.file(configPath).exists())) {
    console.error("Error: 'config.json' not found. Please create one with your SESSION_ID.");
    process.exit(1);
  }
  const config = await Bun.file(configPath).json();
  if (!config.SESSION_ID) {
    console.error("Error: 'SESSION_ID' missing from config.json");
    process.exit(1);
  }
  return config;
}

/**
 * Calculates the default Advent of Code year and day.
 */
function getDefaultAoCDate(): { year: number; day: number } {
  const today = new Date();
  let year = today.getFullYear();
  let day = today.getDate();

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
  return { year, day };
}

/**
 * Gets the absolute path for a given puzzle.
 */
function getPuzzlePath(year: number, day: number): string {
  return path.join(process.cwd(), year.toString(), `day${day}`);
}

/**
 * Creates the year/day directory if it doesn't exist.
 */
function maybeCreateDirectory(year: number, day: number) {
  const yearDirectory = path.join(process.cwd(), year.toString());
  const puzzlePath = getPuzzlePath(year, day);
  if (!fs.existsSync(yearDirectory)) {
    fs.mkdirSync(yearDirectory);
  }
  if (!fs.existsSync(puzzlePath)) {
    fs.mkdirSync(puzzlePath);
  }
}

/**
 * Creates a standard set of headers for AoC requests.
 */
function getAoCHeaders(config: Config): Record<string, string> {
  let headers: Record<string, string> = {
    cookie: `session=${config.SESSION_ID}`,
  };
  if (config.USER_AGENT) {
    headers['user-agent'] = config.USER_AGENT;
  }
  return headers;
}

// -----------------------------------------------------------------
// ## 3. Core Logic (Command Functions)
// -----------------------------------------------------------------

function printHelp() {
  console.log(`
Usage: bun cli.ts [OPTIONS] [COMMAND]

If no command is provided, it will default to 'run'.

Commands:
  run                     Run the puzzle solution for the given day [DEFAULT]
  input                   Download puzzle input for the given day
  copy <ext>              Copies file with given extension from previous day
  submit <part> <answer>  Submit puzzle answer
  prompt                  Download and clean prompt
  list <ext>              List solutions for the year. Can filter by extension
  help                    Print this message

Options:
  -d, --day <DAY>         Puzzle day [default: current day if in December]
  -y, --year <YEAR>       Puzzle year [default: current year]
  -o, --overwrite         Overwrite file if they already exist
  -h, --help              Print this message
`);
}

/**
 * Runs the solution file for the given year and day.
 */
async function runSolution(options: Options) {
  const { year, day } = options;
  console.log(`🏃 Running solution for Year ${year}, Day ${day}...`);
  const puzzlePath = getPuzzlePath(year, day);

  if (!fs.existsSync(puzzlePath)) {
    console.error(`Error: Path does not exist '${puzzlePath}'`);
    process.exit(1);
  }

  let filename = `day${day}.ts`;
  let file = Bun.file(path.join(puzzlePath, filename));
  if (!(await file.exists())) {
    filename = `day${day}.js`;
    file = Bun.file(path.join(puzzlePath, filename));
    if (!(await file.exists())) {
      console.error(`Error: Could not find 'day${day}.ts' or 'day${day}.js' in ${puzzlePath}`);
      process.exit(1);
    }
  }

  console.time('⬅️ Finished in');

  const delay = 3000; // 3 second timeout
  const timeoutId = setTimeout(() => {
    if (proc) proc.kill();
    console.log(`Proc ran over ${delay}ms`);
  }, delay);

  proc = Bun.spawn(['bun', filename], {
    cwd: puzzlePath,
    stdin: 'inherit',
    stdout: 'inherit',
    stderr: 'inherit',
    onExit(proc, exitCode) {
      if (exitCode !== 0) {
        console.log('Proc exited with code:', exitCode);
      } else {
        console.timeEnd('⬅️ Finished in');
      }
      clearTimeout(timeoutId);
    },
  });
}

/**
 * Downloads the puzzle input.
 */
async function downloadInput(options: Options, config: Config) {
  const { year, day, overwrite } = options;
  const filePath = path.join(getPuzzlePath(year, day), 'input.txt');

  if (!overwrite && (await Bun.file(filePath).exists())) {
    console.log(`Input already exists: ${filePath} (Use -o to overwrite)`);
    return;
  }

  const url = `https://adventofcode.com/${year}/day/${day}/input`;
  const response = await fetch(url, { headers: getAoCHeaders(config) });

  if (!response.ok) {
    console.error(`Error downloading input: ${response.statusText}`);
    return;
  }

  maybeCreateDirectory(year, day);
  await Bun.write(filePath, response);
  console.log(`Input downloaded to: ${filePath}`);
}

/**
 * Copies a template file from the most recent previous day.
 */
function copyFile(ext: string | undefined, options: Options) {
  const { year, day, overwrite } = options;
  if (!ext) {
    console.error("Error: 'copy' command requires an extension. (e.g., 'ts')");
    return;
  }

  maybeCreateDirectory(year, day);
  const puzzlePath = getPuzzlePath(year, day);
  const currentPath = path.join(puzzlePath, `day${day}.${ext}`);

  if (!overwrite && fs.existsSync(currentPath)) {
    console.log(`File already exists: ${currentPath} (Use -o to overwrite)`);
    return;
  }

  let isComplete = false;
  const findAndCopy = (searchYear: number, searchDay: number) => {
    while (searchDay >= 1) {
      const file = `day${searchDay}.${ext}`;
      const previousPath = path.join(getPuzzlePath(searchYear, searchDay), file);

      if (fs.existsSync(previousPath)) {
        fs.copyFileSync(previousPath, currentPath);
        console.log(`Copied file ${previousPath} to ${currentPath}`);
        isComplete = true;
        return;
      }
      searchDay--;
    }
  };

  findAndCopy(year, day - 1); // Search current year
  if (!isComplete) {
    findAndCopy(year - 1, 25); // Search previous year
  }

  if (!isComplete) {
    console.log('No previous file to copy from found.');
  }
}

/**
 * Submits a puzzle answer.
 */
async function submit(
  part: string | undefined,
  answer: string | undefined,
  options: Options,
  config: Config
) {
  const { year, day } = options;
  if (!part || !answer) {
    console.error("Error: 'submit' requires <part> and <answer> arguments.");
    return;
  }

  const url = `https://adventofcode.com/${year}/day/${day}/answer`;
  const body = `level=${part}&answer=${answer}`;
  const headers = {
    ...getAoCHeaders(config),
    'Content-Type': 'application/x-www-form-urlencoded',
  };

  const response = await fetch(url, { method: 'POST', body, headers });
  const result = await response.text();

  try {
    const matches = result.match(/<article>(.*?)<\/article>/s);
    if (matches && matches[1]) {
      let message = matches[1]
        .replace(/<[^>]*>/g, ' ')
        .replace(/\s+/g, ' ')
        .trim();
      console.log(`Submit Response: ${message}`);
    } else {
      console.log('Could not parse response:', result);
    }
  } catch (error) {
    console.error('Error parsing submit response:', error);
  }
}

/**
 * Downloads the puzzle prompt as a README.md.
 */
async function downloadPrompt(options: Options, config: Config) {
  const { year, day, overwrite } = options;
  const filePath = path.join(getPuzzlePath(year, day), 'README.md');

  if (!overwrite && fs.existsSync(filePath)) {
    console.log(`Prompt already exists: ${filePath} (Use -o to overwrite)`);
    return;
  }

  const url = `https://adventofcode.com/${year}/day/${day}`;
  const response = await fetch(url, { headers: getAoCHeaders(config) });

  if (!response.ok) {
    console.error(`Error downloading prompt: ${response.statusText}`);
    return;
  }

  maybeCreateDirectory(year, day);
  await Bun.write(filePath, response);
  console.log(`Prompt downloaded to: ${filePath}`);
}

/**
 * Cleans the downloaded prompt HTML into usable Markdown.
 */
function cleanPrompt(options: Options) {
  const { year, day } = options;
  const filePath = path.join(getPuzzlePath(year, day), 'README.md');

  if (!fs.existsSync(filePath)) {
    console.error(`Prompt does not exist: ${filePath}`);
    return;
  }

  let file = fs.readFileSync(filePath, 'utf8');
  let lines = file.split('\n');
  let isMain = false;
  let newLines = [];

  for (const line of lines) {
    if (line.includes('<main>')) {
      isMain = true;
    }
    if (isMain) {
      newLines.push(line);
    }
    if (line.includes('</main>')) {
      isMain = false;
      break;
    }
  }

  fs.writeFileSync(filePath, newLines.join('\n'));
  console.log(`Prompt cleaned: ${filePath}`);
}

/**
 * Lists all solution files for a given year.
 */
async function listFiles(ext: string | undefined, options: Options) {
  const { year } = options;
  const yearPath = path.join(process.cwd(), year.toString());

  if (!fs.existsSync(yearPath)) {
    console.log(`No directory found for year ${year}`);
    return;
  }

  let days = (await fs.promises.readdir(yearPath)) as string[];
  days = days
    .filter(d => d.startsWith('day'))
    .map(d => Number(d.replace('day', '')))
    .sort((a, b) => a - b)
    .map(n => `day${n}`);

  console.log(`Solution files for ${year}:`);
  for (const day of days) {
    const dayPath = path.join(yearPath, day);
    let files = (await fs.promises.readdir(dayPath)) as string[];

    files = files.filter(f => !f.endsWith('.md') && !f.endsWith('.txt') && f !== 'README.md');

    if (ext) {
      files = files.filter(f => f.endsWith(`.${ext}`));
    }

    if (files.length > 0) {
      console.log(`  ${day}: ${files.join(', ')}`);
    }
  }
}

// -----------------------------------------------------------------
// ## 4. Manual Argument Parser
// -----------------------------------------------------------------

/**
 * Manually parses Bun.argv into our clean interface.
 */
function parseArgs(argv: string[]): ParsedArgs {
  const { year: defaultYear, day: defaultDay } = getDefaultAoCDate();
  const options: Options = {
    year: defaultYear,
    day: defaultDay,
    overwrite: false,
    help: false,
  };
  let command: string | null = null;
  const positionalArgs: string[] = [];
  const rawArgs = argv.slice(2); // Skip 'bun' and 'cli.ts'

  for (let i = 0; i < rawArgs.length; i++) {
    const arg = rawArgs[i];

    // Handle flags
    if (arg.startsWith('-')) {
      switch (arg) {
        case '-h':
        case '--help':
          options.help = true;
          break;
        case '-o':
        case '--overwrite':
          options.overwrite = true;
          break;
        case '-y':
        case '--year':
          if (i + 1 < rawArgs.length && !rawArgs[i + 1].startsWith('-')) {
            options.year = parseInt(rawArgs[i + 1], 10);
            i++; // Skip the next arg
          } else {
            console.warn(`Warning: ${arg} flag requires a value.`);
          }
          break;
        case '-d':
        case '--day':
          if (i + 1 < rawArgs.length && !rawArgs[i + 1].startsWith('-')) {
            options.day = parseInt(rawArgs[i + 1], 10);
            i++; // Skip the next arg
          } else {
            console.warn(`Warning: ${arg} flag requires a value.`);
          }
          break;
        default:
          if (arg.startsWith('--year=')) {
            options.year = parseInt(arg.split('=')[1], 10);
          } else if (arg.startsWith('--day=')) {
            options.day = parseInt(arg.split('=')[1], 10);
          } else {
            console.warn(`Warning: Unknown option '${arg}'`);
          }
      }
    } else {
      // Handle command and positional args
      if (command === null) {
        command = arg;
      } else {
        positionalArgs.push(arg);
      }
    }
  }

  // Set default command
  if (command === null && !options.help) {
    command = 'run';
  }

  return {
    command: command || 'help', // Default to 'help' if null
    options,
    args: positionalArgs,
  };
}

// -----------------------------------------------------------------
// ## 5. Main Execution Router
// -----------------------------------------------------------------

/**
 * Main entry point for the script.
 */
async function main() {
  const CONFIG = await loadConfig();
  const { command, options, args } = parseArgs(Bun.argv);

  if (options.help || command === 'help') {
    printHelp();
    return;
  }

  // Route to the correct command function
  switch (command) {
    case 'run':
      await runSolution(options);
      break;
    case 'input':
      await downloadInput(options, CONFIG);
      break;
    case 'copy':
      copyFile(args[0], options);
      break;
    case 'submit':
      await submit(args[0], args[1], options, CONFIG);
      break;
    case 'prompt':
      await downloadPrompt(options, CONFIG);
      cleanPrompt(options); // Run clean after download
      break;
    case 'list':
      await listFiles(args[0], options);
      break;
    default:
      console.error(`Error: Unknown command '${command}'`);
      printHelp();
  }
}

// Run the main function
main().catch(err => {
  console.error('Script failed:', err);
  process.exit(1);
});
