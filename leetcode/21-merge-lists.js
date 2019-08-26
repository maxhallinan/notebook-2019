/*
 * Merge two sorted lists by splicing the nodes of each.
 */

const mergeTwoLists = (l1, l2) => {
  if (!l1 && !l2) {
    return null;
  }

  if (!l1) {
    return l2;
  }

  if (!l2) {
    return l1;
  }

  let fst;
  let snd;

  if (l1.val <= l2.val && l1.next && (l1.next.val <= l2.val)) {
    fst = l1;
    snd = l1.next;
    l1 = l1.next.next;
  } else if (l2.val <= l1.val && l2.next && (l2.next.val <= l1.val)) {
    fst = l2;
    snd = l2.next;
    l2 = l2.next.next;
  } else if (l1.val <= l2.val) {
    fst = l1;
    snd = l2;
    l1 = l1.next;
    l2 = l2.next;
  } else if (l2.val <= l1.val) {
    fst = l2;
    snd = l1;
    l1 = l1.next;
    l2 = l2.next;
  }

  fst.next = snd;
  snd.next = mergeTwoLists(l1, l2);

  return fst;
};
function ListNode(val) {
  this.val = val;
  this.next = null;
}
const one = new ListNode(1);
const two = new ListNode(2);
const three = new ListNode(3);
const four = new ListNode(4);
one.next = two;
two.next = three;
three.next = four;

const five = new ListNode(5);
const six = new ListNode(6);
const seven = new ListNode(7);
const eight = new ListNode(8);
five.next = six;
six.next = seven;
seven.next = eight;

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
console.log(listToArray(mergeTwoLists(two, one)));
