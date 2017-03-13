function generateGridViaForLoops(x, y) {
  let currentRow = '';

  for(let j = 0; j < y; j++) {
    for(let i = 0; i < x; i++) {
      currentRow += Math.min(i + 1, j + 1, x - i, y - j);
    }
    console.log(currentRow);
    currentRow = '';
  }
}

module.exports.generateGridViaForLoops = generateGridViaForLoops;

function generateGridRecursively(x, y) {
  const width = x;
  const height = y;
  const grid = recursiveGrid(x, y);

  function recursiveGrid(x, y) {
    if (y > 1) {
      return recursiveGrid(x, y - 1) + recursiveRow(x, y) + '\n';
    }
    return recursiveRow(x, y) + '\n';
  }

  function recursiveRow(x, y) {
    if (x > 1) {
      return recursiveRow(x - 1, y) + '' + Math.min(x, y, width + 1 - x, height + 1 - y);
    }
    return Math.min(x, y, width + 1 - x, height + 1 - y) + '';
  }

  return console.log(grid);
}

module.exports.generateGridRecursively = generateGridRecursively;
