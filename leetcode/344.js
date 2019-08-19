/* 
 * LeetCode 344. Reverse String
 * Reverse a string (represented as a char array).
 * This solution uses the two-pointer technique.
 * Time is O(N)
 * Space is O(1)
 * This is in the 87.01 percentile for time.
 */
const reverseCharArr = (s) => {
  let a = 0;
  let b = s.length - 1;
  let temp;
  while (a < b) {
    temp = s[a];
    s[a] = s[b];
    s[b] = temp;
    a++;
    b--;
  }
};
