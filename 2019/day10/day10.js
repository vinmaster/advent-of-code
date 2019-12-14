const fs = require('fs');
const path = require('path');

function inGrid(grid, x, y) {
  return x > 0 && x < grid[0].length && y > 0 && y < grid.length;
}

function atan2Degrees(y, x) {
  return radiansToDegrees(Math.atan2(y, x));
}

function radiansToDegrees(radians) {
  return radians * 180 / Math.PI;
}

function distance(a, b) {
  return Math.abs(b.x - a.x) + (Math.abs(b.y - a.y));
}

function gcd(n, m) {
  let r = 0;
  while (n !== 0) {
    r = m % n;
    m = n;
    n = r;
  }
  return m;
};

function pointsBetween(grid, x1, y1, x2, y2) {
  let [dx, dy] = [x2 - x1, y2 - y1];
  if (dx === 0 && dy === 0) { return []; }
  const points = [];
  let divisor = gcd(dx, dy);
  if (divisor < 0) { divisor *= -1; }
  for (let i = 1; i < Math.abs(divisor); i++) {
    // let horizontalDirection = dx < 0 ? -i : i;
    // let verticalDirection = dy < 0 ? -i : i;
    let horizontalDirection = i;
    let verticalDirection = i;
    let target = [
      x1 + (dx / divisor * horizontalDirection),
      y1 + (dy / divisor * verticalDirection),
    ];

    if (!inGrid(grid, target[0], target[1])) { continue; }

    if (grid[target[1]][target[0]] !== '.') {
      // There is something here
      points.push(target);
    }
  }
  return points;
}

function part1(input) {
  const gridInput = input
    .trim()
    .split('\n');
  const grid = gridInput.map(row => row.trim().split(''));
  const grid2 = gridInput.map(row => row.trim().split(''));
  let asteroids = [];

  grid.forEach((row, y) => {
    row.forEach((cell, x) => {
      if (cell !== '.') {
        asteroids.push({ x, y });
      }
    });
  });
  asteroids = asteroids.map(({ x: x1, y: y1 }) => {
    const set = new Set();
    // Check all other points on the map
    let count = 0;
    asteroids.forEach(({ x: x2, y: y2 }) => {
      if (!(x1 === x2 && y1 === y2)) {
        points = pointsBetween(grid, x1, y1, x2, y2);
        if (points.length === 0) {
          count++;
        }
      }
    });
    return {
      x: x1,
      y: y1,
      asteroids: count,
    };
  });
  return asteroids.sort((a, b) => b.asteroids - a.asteroids)[0].asteroids;
}

/*
atan2 degrees
    90
180 x 0
   -90
swap atan2 x and y to get degrees
    0
-90 x 90
   180
*/
function part2(input) {
  const gridInput = input
    .trim()
    .split('\n');
  const grid = gridInput.map(row => row.trim().split(''));
  const grid2 = gridInput.map(row => row.trim().split(''));
  let asteroids = [];
  const station = { x: 17, y: 22 }

  grid.forEach((row, y) => {
    row.forEach((cell, x) => {
      if (cell !== '.') {
        asteroids.push({ x, y });
      }
    });
  });

  const asteroidsToDestroy = new Map();
  let count = 0;
  // Cap at 10 cycles
  for (let i = 0; i < 10; i++) {
    asteroids.forEach((a) => {
      if (!(station.x === a.x && station.y === a.y)) {
        // Flip x and y and adjust to have 0 degrees as up
        const degrees = (atan2Degrees(a.x - station.x, station.y - a.y) + 360) % 360;
        const prevA = asteroidsToDestroy.get(degrees);
        a.distance = distance(station, a);
        if (prevA) {
          if (a.distance < prevA.distance) {
            asteroidsToDestroy.set(degrees, a);
          }
        } else {
          asteroidsToDestroy.set(degrees, a);
        }
      }
    });
    // Sort by degrees
    const asteroidsList = [...asteroidsToDestroy];
    asteroidsList.sort((a, b) => a[0] - b[0]);
    for (const a of asteroidsList) {
      const index = asteroids.findIndex(b => a[1].x === b.x && a[1].y === b.y);
      if (index !== -1) {
        count += 1;
        if (count === 200) {
          return a[1].x * 100 + a[1].y;
        }
        asteroids.splice(index, 1);
      }
    }
  }
}

let input = fs.readFileSync(path.resolve(__dirname, './input.txt'), 'utf8');
console.log('day10 part1:', part1(input));
console.log('day10 part2:', part2(input));

/*

function other(input) {
  const asteroids = input
    .split('\n')
    .reduce((objects, line, y) => {
      line.trim().split('').forEach((space, x) => {
        if (space === '#') {
          objects.push({ x, y });
        }
      });

      return objects;
    }, []);

  const count = asteroids
    .map(({ x: x1, y: y1 }) => {
      const angles = new Set();

      asteroids.forEach(({ x: x2, y: y2 }) => {
        if (!(x1 === x2 && y1 === y2)) {
          angles.add(Math.atan2(y2 - y1, x2 - x1));
        }
      });

      return {
        asteroids: angles.size,
        x: x1,
        y: y1,
      };
    }).sort((a, b) => b.asteroids - a.asteroids);
    console.log(count[0]);
    return count;
};

*/