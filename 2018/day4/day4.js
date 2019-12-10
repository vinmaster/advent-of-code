const fs = require('fs');
const path = require('path');

const ENTRY_REGEX = /^\[\d{4}\-(\d{2}\-\d{2})\s(\d{2}:\d{2})\]\s(?:Guard.*#)?(\d{1,})?.*(falls|wakes|begins).*/;

function sortEntries(entries) {
  entries.sort((a, b) => {
    const [,date1, time1] = a.match(ENTRY_REGEX);
    const [,date2, time2] = b.match(ENTRY_REGEX);
    if (date1 + time1 < date2 + time2) { return -1; }
    else if (date1 + time1 > date2 + time2) { return 1; }
    else { return 0; }
  });
}

function getTimeSlept(start, end) {
  return parseInt(end.replace(':', ''), 10) - parseInt(start.replace(':', ''), 10);
}

function updateMinuteMap(map, start, end) {
  start = parseInt(start.replace(':', ''), 10);
  end = parseInt(end.replace(':', ''), 10);
  for (; start <= end; start++) {
    map[start.toString()] = map[start.toString()] || 0;
    map[start.toString()] += 1;
    if (map[start.toString()] % 100 === 60) {
      start += 100;
      start -= 60;
    }
  }
}

function guardSleeptimes(entries) {
  const sleepMap = {};
  let currentGuard = null;
  let sleepStart = null;
  for (const e of entries) {
    const [,date, time, guard, action] = e.match(ENTRY_REGEX);
    if (action === 'begins') {
      currentGuard = guard;
    } else if (action === 'falls') {
      sleepStart = time;
    } else if (action === 'wakes') {
      sleepMap[currentGuard] = sleepMap[currentGuard] || { sleepTime: 0, minuteMap: {}};
      // Total time slept
      sleepMap[currentGuard].sleepTime += getTimeSlept(sleepStart, time);
      // Mark the minutes asleep
      updateMinuteMap(sleepMap[currentGuard].minuteMap, sleepStart, time);
    } else {
      throw new Error(`Invalid entry: ${action}`);
    }
  }
  return sleepMap;
}

function part1(input) {
  const entries = input
    .trim()
    .split('\n');
  
  sortEntries(entries);
  const sleepMap = guardSleeptimes(entries);

  let maxSleepTime = null;
  let target = {};
  for (const [guard, { sleepTime }] of Object.entries(sleepMap)) {
    if (maxSleepTime === null || sleepTime > maxSleepTime) {
      maxSleepTime = sleepTime;
      target = {
        guard, sleepTime
      }
    }
  }
  let maxMinuteSleepTime = null;
  for (const [minute, sleepTime] of Object.entries(sleepMap[target.guard].minuteMap)) {
    if (maxMinuteSleepTime === null || sleepTime > maxMinuteSleepTime) {
      maxMinuteSleepTime = sleepTime;
      target.minute = minute;
    }
  }
  return target.guard * target.minute;
}

function part2(input) {
  const entries = input
    .trim()
    .split('\n');
  
  sortEntries(entries);
  const sleepMap = guardSleeptimes(entries);

  let allGuardsmaxMinuteSleepTime = null;
  let target = {};

  for (const guard of Object.keys(sleepMap)) {
    for (const [minute, sleepTime] of Object.entries(sleepMap[guard].minuteMap)) {
      if (allGuardsmaxMinuteSleepTime === null || sleepTime > allGuardsmaxMinuteSleepTime) {
        allGuardsmaxMinuteSleepTime = sleepTime;
        target.guard = guard;
        target.minute = minute;
      }
    }
    
  }
  return target.guard * target.minute;
}

const input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day4 part1:', part1(input));
console.log('day4 part2:', part2(input));
