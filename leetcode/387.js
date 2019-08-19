/*
 * LeetCode 387. First unique character in a string
 * Find the index of the first unique character in a string.
 * This solution is in the 87% percentile for time.
 * Time is O(N^2)
 * Space is O(1)
 */
var firstUniqChar = (s) => {
  let isDup;

  for (let i = 0; i < s.length; i++) {
    isDup = false;
    for (let j = 0; j < s.length; j++) {
      if (s.charAt(i) === s.charAt(j) && i!== j) {
        isDup = true;
        break;
      }
    }
    
    if (!isDup) {
      return i;
    }
  }

  return -1;
};

/*
 * Here is another solution.
 * This is in the 87% percentile as well.
 * Time is O(2N)
 * Space is O(N)
 */ 
var firstUniqChar2 = (s) => {
  const occurrences = {};

  for (let i = 0; i < s.length; i++) {
    const c = s.charAt(i);
    occurrences[c] = occurrences[c] ? occurrences[c] + 1 : 1;
  }

  for (let i = 0; i < s.length; i++) {
    const o = occurrences[s.charAt(i)];
    if (o === 1) {
      return i;
    }
  }

  return -1;
};
