 
import fs from 'fs';

// input
const input = fs.readFileSync('input-day03.txt', 'utf8');

// part 1
const add = (a, b) => a + b;
const calcTotal = function(x, pattern) {
  const matches = [...x.matchAll(pattern)];
  const values = matches.map(x => x[1] * x[2]);
  const totalValue = values.reduce(add);
  return totalValue;  
}
const mul_pattern = /mul\((\d{1,3}),(\d{1,3})\)/g;
const total = calcTotal(input, mul_pattern);
console.log(total);

// part 2
const calcTotalDos = function(x, pattern) {
  const parts = input.split(/(do\(\)|don't\(\))/);
  const initialState = { total: 0, isEnabled: true };
  const y = parts.reduce(
    ({ total, isEnabled }, part) => {
      if (part === 'do()') return { total, isEnabled: true };
      if (part === "don't()") return { total, isEnabled: false };
      if (isEnabled) {
        const sum = calcTotal(part, pattern);
        return { total: total + sum, isEnabled };
      }
      return { total, isEnabled };
    }, initialState
  )
  return y.total;
};
const totalDos = calcTotalDos(input, mul_pattern)
console.log(totalDos);
