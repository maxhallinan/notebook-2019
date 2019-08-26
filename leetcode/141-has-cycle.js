/*
 * 141. Determine if a linked list has a cycle in it.
 * This solution is in the 53.60th percentile for speed. 
 * This solution is in the 37.50th percentile for memory.
 */
const hasCycle = (head) => {
  let current = head;
  while (current) {
    if (current.visited) {
      return true;
    }
    current.visited = true;
    current = current.next;
  }
  return false;
};

/*
 * This solution is slower than most submissions. They didn't give it a 
 * percentile.
 */
const hasCycle2 = (head) => {
  const visited = [];
  let current = head;
  while(current) {
    if (visited.indexOf(current) > -1) {
      return true;
    }
    visited.push(current);
    current = current.next;
  }
  return false;
};

/*
 * This solution is faster than 92.22% of submissions.
 * This solution uses less memory than 56.25% of submissions.
 */
const hasCycle3 = (head) => {
  if (!head) {
    return false;
  }
  let slow = head;
  let fast = head.next ? head.next.next : null;
  while (slow && fast) {
    if (slow === fast) {
      return true;
    }
    slow = slow.next;
    fast = fast.next ? fast.next.next : null;
  }
  return false;
};

function ListNode(val) {
  this.val = val;
  this.next = null;
}

const a = new ListNode('a');
const b = next = new ListNode('b');
a.next = b;
b.next = a;

const x = new ListNode('x');
const y = new ListNode('y');
const z = new ListNode('z');
x.next = y;
y.next = z;

const one = new ListNode(1);

const t1 = new ListNode(3);
const t2 = new ListNode(2);
const t3 = new ListNode(0);
const t4 = new ListNode(-4);
t1.next = t2;
t2.next = t3;
t3.next = t4;
t4.next = t2;

console.log(hasCycle3(one));
console.log(hasCycle3(a));
console.log(hasCycle3(x));
console.log(hasCycle3(t1));
