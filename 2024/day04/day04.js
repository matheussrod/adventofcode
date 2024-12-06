
import fs from 'fs';

// input
const input = fs.readFileSync('input-day04.txt', 'utf8')
  .split('\r\n')
  .map(x => x.split(''));


// part 1
const add = (a, b) => a + b;
const isXmas = x => {
  if (x.length === 4) {
    return x.reduce(add) === 'XMAS' ? 1 : 0;
  } else {
    return 0;
  }
};

const findXmas = (input, i, j) => {
  const right = isXmas(input[i].slice(j, j + 4));
  const left = isXmas(input[i].slice(j - 3, j + 1).reverse());
  const down = isXmas(input.slice(i, i + 4).map(x => x[j]));
  const up = isXmas(input.slice(i - 3, i + 1).map(x => x[j]).reverse());
  
  const diag = [0, 1, 2, 3];
  const rightUp = isXmas(input.slice(i - 3, i + 1).reverse().map((x, z) => x[j + diag[z]]));
  const rightDown = isXmas(input.slice(i, i + 4).map((x, z) => x[j + diag[z]]));
  const leftUp = isXmas(input.slice(i - 3, i + 1).reverse().map((x, z) => x[j - diag[z]]));
  const leftDown = isXmas(input.slice(i, i + 4).map((x, z) => x[j - diag[z]]));

  return right + left + down + up + rightUp + leftUp + rightDown + leftDown;
}

const xmas = input.map((x, i) => x.map((y, j) => y !== 'X' ? 0 : findXmas(input, i, j)));
const xmasCount = xmas.map(x => x.reduce(add)).reduce(add);
console.log(xmasCount);

// part 2
const isXMas = x => {
  if (x.length === 3) {
    const value = x.reduce(add);
    return value === 'MAS' || value === 'SAM' ? true : false;
  } else {
    return false;
  }
};

const findXMas = (input, i, j) => {
  const diag = [-1, 0, 1];
  const right = isXMas(input.slice(i - 1, i + 2).map((x, z) => x[j + diag.reverse()[z]]));
  const left = isXMas(input.slice(i - 1, i + 2).map((x, z) => x[j - diag[z]]));
  return right && left ? 1 : 0;
}

const xMas = input.map((x, i) => x.map((y, j) => y !== 'A' ? 0 : findXMas(input, i, j)));
const xMasCount = xMas.map(x => x.reduce(add)).reduce(add);
console.log(xMasCount);
