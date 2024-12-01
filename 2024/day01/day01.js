
import fs from 'fs';

// input
const input = fs.readFileSync('input-day01.txt', 'utf8')
  .split('\r\n')
  .map(x => x.split('   '));

const locationId1 = input.map(x => parseInt(x[0]));
const locationId2 = input.map(x => parseInt(x[1]));

// part 1
const zip = (a, b) => a.map((v, i) => [v, b[i]]);
const zipLocations = zip(
  locationId1.toSorted(),
  locationId2.toSorted()
)

const add = (a, b) => a + b;
const distance = zipLocations.map(x => Math.abs(x[0] - x[1]));
const totalDistance = distance.reduce(add);
console.log(totalDistance);

// part 2
const countOccurrences = function(obj, value) {
  if (obj[value]) {
    obj[value] += 1;
  } else {
    obj[value] = 1;
  }
  return obj;
}

const countLocation2 = locationId2.reduce(countOccurrences, {})
const getSimilarity = (a, b) => b[a] ? a * b[a] : 0;
const similarity = locationId1.map(x => getSimilarity(x, countLocation2));
const totalSimilarity = similarity.reduce(add);
console.log(totalSimilarity);
