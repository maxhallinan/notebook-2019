/*
 * Palindrome linked list
 * This is in the 99.95th percentile for speed and the 40th percentile for 
 * memory.
 */
const isPalindrome = (head) => {
  let current = head;
  const items = {};
  let length = 0;

  while (current) {
    length++;
    items[length] = current.val;
    current = current.next;
  }

  for (let i = 0; i <= length / 2; i++) {
    if (items[i + 1] !== items[length - i]) {
      return false;
    }
  }

  return true;
};

function ListNode(val) {
  this.val = val;
  this.next = null;
}

const arrToList = ([head, ...tail]) => {
  const first = new ListNode(head);

  const last = tail.reduce((prev, val) => {
    const n = new ListNode(val);
    prev.next = n;
    return n;
  }, first);

  return first;
}; 

const listToArr = (head) => {
  const arr = [];
  let curr = head;
  while (curr) {
    arr.push(curr.val);
    curr = curr.next;
  }
  return arr;
};

const palindrome = arrToList(['a','b','c','c','b','a']);
const notPalindrome = arrToList(['a','b','c']);
console.log(listToArr(palindrome));
console.log('is palindrome ', isPalindrome(palindrome));
console.log('is not palindrome ', isPalindrome(notPalindrome));
