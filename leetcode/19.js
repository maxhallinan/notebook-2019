/*
 * Remove Nth node from the end of the list
 * Given a linked list, remove the n-th node from the end of the list and 
 * return its head.
 * This solution is in the 98th percentile for time and the 13.64th percentile
 * for space.
 */
const removeNthFromEnd = function(head, n) {
  let dict = {};
  let current = head;
  let k = 0;

  while (current !== null) {
    // store each node in a table
    dict[k] = current;
    // count the nodes in the list
    k++;    
    current = current.next;
  }

  // the node preceding the target node
  const before = dict[k - n - 1];
  // the n-th node from the end of the list
  const target = dict[k - n];
  // the node following the target node
  const after = dict[k - n + 1];

  // a list of 1
  if (!before && !after) {
    head = null;
  }

  // target is first item in the list
  if (!before && after) {
    target.val = after.val;
    target.next = after.next;
  }

  // target is last item in the list
  if (before && !after) {
    before.next = null;
  }

  // target is somewhere in the middle of the list
  if (before && after) {
    before.next = after;
  }

  return head;
}
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
console.log(listToArray(one));
removeNthFromEnd(one, 1);
console.log(listToArray(one));
