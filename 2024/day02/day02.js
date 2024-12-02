
import fs from 'fs';

// input
const input = fs.readFileSync('input-day02.txt', 'utf8')
  .split('\r\n')
  .map(x => x.split(' ').map(Number));

// part 1
const calcDiff = x => x.slice(1).map((v, i) => v - x[i])

const isSafe = function(report) {
  const diff = calcDiff(report);
  const allDiffsValid = diff.every(diff => Math.abs(diff) >= 1 && Math.abs(diff) <= 3);
  const isInc = diff.every(diff => diff > 0);
  const isDec = diff.every(diff => diff < 0);
  return allDiffsValid && (isInc || isDec);
};

const safeReports = input.filter(isSafe);
const totalSafeReports = safeReports.length;
console.log(totalSafeReports);

// part 2
const canBeMadeSafe = (report) => 
  report.some((_, i) => isSafe(report.filter((_, j) => j !== i)));

const canBeSafeReports = input.filter(canBeMadeSafe);
const totalCanBeSafeReports = canBeSafeReports.length;
console.log(totalCanBeSafeReports);
