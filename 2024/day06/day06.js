
import fs from 'fs';

// input
const input = fs.readFileSync('input-day06.txt', 'utf8')
  .split('\r\n')
  .map(line => line.split(''));

// part 1
const DIRECTIONS = ['^', '>', 'v', '<'];
const DIFFS = { '^': [-1, 0], '>': [0, 1], 'v': [1, 0], '<': [0, -1] };

const turnRight = (currentDirection) => {
  const currentIndex = DIRECTIONS.indexOf(currentDirection);
  return DIRECTIONS[(currentIndex + 1) % 4];
};

const moveForward = (position, direction) => {
  const [dx, dy] = DIFFS[direction];
  return [position[0] + dx, position[1] + dy];
};

const isObstacle = (grid, [x, y]) => grid[x] && grid[x][y] === '#';
const isOutsideGrid = (grid, [x, y]) => x < 0 || y < 0 || x >= grid.length || y >= grid[0].length;

let startPos, startDirection;
input.forEach((row, x) => {
  row.forEach((cell, y) => {
    if (DIRECTIONS.includes(cell)) {
      startPos = [x, y];
      startDirection = cell;
    }
  });
});

const simulateGuard = (grid, startPos, startDirection) => {
  let position = startPos;
  let direction = startDirection;
  const visited = new Set();

  while (!isOutsideGrid(grid, position)) {
    visited.add(position.toString());
    const nextPosition = moveForward(position, direction);
    if (isObstacle(grid, nextPosition)) {
      direction = turnRight(direction);
    } else {
      position = nextPosition;
    }
  }
  return visited.size;
};

const uniquePositions = simulateGuard(input, startPos, startDirection);
console.log(uniquePositions);
