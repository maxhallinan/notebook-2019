# Linked Lists

## Find a cycle in a linked list

**Floyd's cycle-finding algorithm**

- 2 references to the list
- Move the references by different speeds:
  - Slow: move 1 node
  - Fast: move 2 nodes
- If the linked list has a cycle, then these two references will eventually point
  to the same node.
- Otherwise, one of the references will become `null`.

```javascript
function hasCycle(head) {
  if (!head) {
    return false;
  }

  let slow = head;
  let fast = head.next;

  while (slow && fast) {
    if (slow === fast) {
      return true;
    }

    slow = head.next;
    fast = slow ? head.next.next : null;
  }
}
```

**Problem 1**

Originally I had this:

```javascript
slow = slow.next;
fast = slow ? slow.next.next : null;
```

but the function got stuck in an infinite loop.
The problem is that `fast` and `slow` move at the same speeds.
`fast` is just always one ahead of `slow`.
The function never reaches the terminal condition.

The correct way is this:

```javascript
slow = slow.next;
fast = fast.next ? fast.next.next : null;
```

Now `fast` is moving at 2x the speed of `slow`.

**Problem 2**

I also initialized them like this:

```javascript
let slow = head;
let fast = head.next;
```

When initialized thus, the function produced the desired result but did so 
slowly.
It had to go through more iterations than necessary to reach the terminal 
condition.

By starting `fast` two ahead of `slow`, the terminal condition is reached faster.
In my case it was a difference of 68ms versus 60ms.

```javascript
let slow = head;
let fast = head.next ? head.next.next : null;
```
