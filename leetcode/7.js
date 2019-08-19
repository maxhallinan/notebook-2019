/* LeetCode 7. Reverse integer
 * Given a 32-bit signed integer, reverse the digits of the integer.
 * This is time O(N)
 * This is space O(N)
 * This is in the 43 percentile for time and the 36.3% for memory.
 */
var reverseInt = (int) => {
  const min = -(2**31)
  const max = (2**31) - 1

  let s = ('' + int).split('');
  let a = s[0] === '-' ? 1 : 0;
  let b = s.length - 1;
  let temp;

  while(a < b) {
    temp = s[a];    
    s[a] = s[b];
    s[b] = temp;
    a++
    b--;
  }

  const i = s.join('') - 0;

  if (i < min || i > max) {
    return 0;
  }

  return i;
};

console.log(reverseInt(-123));
// -321
console.log(reverseInt(120));
// 21

/*
 * This is a second attempt.
 * This is actually quite slow: in the 13th percentile for time.
 * In the 43rd percentile for memory.
 */
var reverseInt2 = (n) => {
  let s = x.toString().split('').reverse();
  let i = s[s.length - 1] === '-' 
    ? -(parseInt(s.slice(0, s.length - 1).join(''))) 
    : parseInt(s.join(''));
        
  if (i < -(2**31) || i > (2**31) - 1) {
    return 0;
  }

  return i;    
};

/*
 * Here's a nice one that I isn't mine.
 * This is in the 61st percentile for time and the 49th percentile for memory.
 */

var reverseInt3 = (n) => {
  const s = Math.abs(n).toString().split('').reverse().join('');
  const i = Number(n < 0 ? '-' + s : s);

  if (i < -(2**31) || i > (2**31) - 1) {
    return 0;
  }

  return i;    
}
