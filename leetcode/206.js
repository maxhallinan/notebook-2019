/*
 * Reverse a singly linked list.
 * 31st percentile for speed.
 * 85th percentile for memory.
 */
const reverseList = (head) => {
  if (head === null) {
    return null;
  }
  
  let current = head;
  let next = head.next;
  let reversed = null;

  while (current !== null) {
    next = current.next;
    current.next = reversed;
    reversed = current;    
    current = next;
  }

  return reversed;
};
function ListNode(val) {
  this.val = val;
  this.next = null;
}
const four = new ListNode(4);
const three = new ListNode(3);
const two = new ListNode(2);
const one = new ListNode(1);
one.next = two;
two.next = three;
three.next = four;

const listToArray = (head) => {
  const result = [];
  let current = head;

  while (current !== null) {
    result.push(current.val);
    current = current.next;
  }

  return result;
};
// [1,2,3,4]
console.log(listToArray(one));
// [4,3,2,1]
console.log(listToArray(reverseList(one)));
