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

scaleDownBy(10, 1.333, 1).sort((a, b) => b - a).forEach(r => console.log(r));
