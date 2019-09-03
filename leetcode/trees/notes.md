# Trees

## Traversal of a binary tree

There are two strategies for traversing a binary tree:

- depth-first: visit all nodes on the left side of the tree before visiting the
  right side of the tree.
- breadth-first: visit all sibling nodes, then go to the next level.

There are three kinds of depth first traversals.
Each visits the root node in a different order relative to the left and right
subtrees.

- preorder traversal: left, root, right
- inorder traversal: root, left, right
- postorder traversal: left, right, root

### TreeNode

```javascript
class TreeNode {
  constructor(val) {
    this.val = val;
    this.left = null;
    this.right = null;
  }
}
```

### Breadth-first traversal

Breadth first traversals are implemented with recursion:

```javascript
function preorder(visit, root) {
  if (!root) {
    return root;
  }

  // left, root, right
  preorder(visit, root.left);
  visit(root.val);
  preorder(visit, root.right);
}
```

```javascript
function inorder(visit, root) {
  if (!root) {
    return root;
  }

  // root, left, right
  visit(root.val);
  inorder(visit, root.left);
  inorder(visit, root.right);
}
```

```javascript
function postorder(visit, root) {
  if (!root) {
    return root;
  }

  // left, right, root
  postorder(visit, root.left);
  postorder(visit, root.right);
  visit(root.val);
}
```

The left side of the tree is always visited before the right side.

### Level-order traversal

Level-order traversal is implemented with a queue.

```javascript
function levelorder(visit, root) {
  const q = [root.val];
  while(q.length) {
    const n = q.shift();
    visit(n);
    if (n.left) {
      root.push(n.left);
    }
    if (n.right) {
      root.push(n.right);
    }
  }
}
```

Each node is visited in the order it enters the queue.
After visiting a node, you push it's children (if they exist) into the queue.
The root note is first.
After visiting the root node, the left and right nodes are pushed into the 
queue.
The next node to visit is the left child of the root node.
Then its children are pushed into the queue.
The third node to visit is the right child of the root node. 
And its children are pushed into the queue.
Now we're back to the left child of the left child of the root node, which was
visited second.
And so on and so forth.

## Binary Search Tree (BST)

**Motivating example**

- User logs into Facebook and Facebook wants to check if the username exists.
- Fastest way to search through a big array of usernames is binary search.
- For each new username, the array must be re-sorted.
- Binary search trees are a way to insert the new username in the right place 
  without having to re-sort everything.

Structure

- For each node, the nodes in the left subtree are less than the node
- Nodes in the right subtree are greater than the node

Searching through a binary tree means:

- If item is less than current node, go left
- If item is greater than current node, go right

This takes:

- O(log n) on average
- O(n) for worst case

Arrays are only O(log n) for search. 
They're O(n) for insert and delete.

BSTs are O(log n) for search, insert, and delete.
But BSTs don't give you random access.

Some different types of trees:

- B-trees
- Red-black trees
- Heaps
- Splay trees
