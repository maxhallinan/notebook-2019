/* 237. Delete Node in a Linked List
 * Write a function that deletes a node, given only access to that node.
 * This solution is in the 99th percentile for speed but only in the 17th 
 * percentile for memory.
 */
const deleteNode = (node) => {
  node.val = node.next.val;
  node.next = node.next.next;
};

function ListNode(val) {
  this.val = val;
  this.next = null;
}
const four = new ListNode(4);
const three = new ListNode(3);
const two = new ListNode(2);
const one = new ListNode(1);
three.next = four;
two.next = three;
one.next = two;
// [1,2,3,4]
const nums = [one.val, one.next.val, one.next.next.val, one.next.next.next.val];
console.log(nums);
deleteNode(three);
// [1,2,4]
console.log([one.val, one.next.val, one.next.next.val]);
