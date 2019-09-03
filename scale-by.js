const fs = require('fs');

const scaleUpBy = (n, factor, start) => {
  const results = [start];
  while(0 < n) {
    results.push(results[results.length - 1] * factor);
    n--;
  }

  return results;
};

const scaleDownBy = (n, factor, start) => {
  const results = [start];
  while(0 < n) {
    results.push(results[results.length - 1] / factor);
    n--;
  }

  return results;
};

// scaleDownBy(10, 1.333, 1).sort((a, b) => b - a).forEach(r => console.log(r));

const round = (n) => Math.round(n * 100) / 100;

const modScale = (ratio, emUnit, bases) => {
  const max = 2000;
  const min = 1;

  return bases.reduce((accum, base) => {
    let xs = [[base, round(base / emUnit)]];

    let gt = base;
    while (gt <= max) {
      const px = gt * ratio;
      const em = px / emUnit;
      xs.push([round(px), round(em)]);
      gt = px;
    };

    let lt = base;
    while (min <= lt) {
      const px = lt / ratio;
      const em = px / emUnit;
      xs.push([round(px), round(em)]);
      lt = px;
    }

    return [...accum, ...xs];
  }, []).sort(([a], [b]) => b - a);
};

const scale = modScale(1.125, 18, [18, 650]).map(([px, em]) => {
  return `${em}em ${px}px`;
}).join('\n');

fs.writeFile('./modular-scale', scale, (err) => {
  console.log(err);
});
