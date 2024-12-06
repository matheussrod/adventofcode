
import fs from 'fs';

// input
const input = fs.readFileSync('input-day05.txt', 'utf8')
  .split('\r\n\r\n');

const rules = input[0]
  .split('\r\n')
  .map(x => {
    const [y, z] = x.split('|');
    return { [y]:z };
  });

const updates = input[1]
  .split('\r\n')
  .map(x => x.split(','));

// part 1
const add = (a, b) => a + b;

const mappingRules = (dict, x) => {
  const key = Object.keys(x)[0];
  const value = Object.values(x)[0];
   if (Object.keys(dict).length === 0) {
    return { ...dict, [key]: [value] };
  } else if (key in dict) {
    return { ...dict, [key]: [...dict[key], value] };
  } else {
    return { ...dict, [key]: [value] };
  }
};

const countOrder = (update, rules) => {
  return update.map(x => {
    const rule = rules[x];
    const pos = update.map(
      y => rule 
        ? rule.filter(z => z === y).length 
        : 0
    ).reduce(add);
    return { [x]: rule ? pos : 0 };
  })
};

const checkOrder = x => x.every((x, i, a) => {
  if (i === 0) return true;
  return Object.values(x)[0] <= Object.values(a[i - 1])[0];
});

const backToList = x => x.map(x => x.map(y => Number(Object.keys(y)[0])));
const calcMiddle = x => backToList(x)
  .map(x => x[Math.floor(x.length/2)])
  .reduce(add);

const mappedRules = rules.reduce(mappingRules, {});
const updatesCounters = updates.map(x => countOrder(x, mappedRules));
const validUpdates = updatesCounters.filter(checkOrder);
const totalMiddle = calcMiddle(validUpdates);
console.log(totalMiddle);

// part 2
const fixUpdate = x => 
  [...x].sort((x, y) => 
  Object.values(y)[0] - Object.values(x)[0]);

const invalidUpdates = updatesCounters.filter(x => !checkOrder(x));
const newUpdates = invalidUpdates.map(fixUpdate);
const newTotalMiddle = calcMiddle(newUpdates);
console.log(newTotalMiddle);
