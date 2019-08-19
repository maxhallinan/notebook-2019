/* Two sum
 * Given an array of integers, return indices of the two numbers such that they add up to a specific taget
 * Assume that each input has exactly one solution
 * Don't use the same element twice
 * This version is O(N^2) for time, O(1) for memory
 * This is in the 16th percentile for time and the 29th percentile for memory
 */

const twoSum = (nums, target) => {
  for (let i = 0; i < nums.length; i++) {
    for (let k = 0; k < nums.length; k++) {
      if (i !== k && nums[i] + nums[k] === target) {
        return [i,k];
      }
    }
  }
}

// this is broken
const twoSum2 = (nums, target) => {
  let a = 0;
  let b = nums.length - 1;
  while (a < nums.length) {
    if (nums[a] + nums[b] === target) {
      return [a,b];
    }
    a++;
    b--;
  }
}

const twoSum3 = (nums, target) => {
  const x = {};

  for (let i = 0; i < nums.length; i++) {
    if (i <= target) {
      x[i] = nums[i];
    }
  }

  for (let i = 0; i < nums.length; i++) {
  }
};

const input = [2,7,11,15];
const target = 9;
const output = [0,1];
console.log(twoSum2([3,2,3],6));
