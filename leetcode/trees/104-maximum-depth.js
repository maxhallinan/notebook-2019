/*
 * Given a binary tree, find its maximum depth.
 * The maximum depth is the number of nodes along the longest path from the root
 * node down to the farthest leaf node.
 * This solution is in the 53.51 percentile since it calculates the depth for 
 * each branch of the tree.
 */
const maxDepth = (root) => {
  if (!root) {
    return 0;
  }

  const leftDepth = 1 + maxDepth(root.left); 
  const rightDepth = 1 + maxDepth(root.right);
  const depth = leftDepth < rightDepth ? rightDepth : leftDepth;

  return depth;
};

const maxDepth2 = (root) => {
  if (!root) {
    return 0;
  }
  return Math.max(maxDepth(root.left), maxDepth(root.right)) + 1;
};

function TreeNode(val) {
  this.val = val;
  this.left = null;
  this.right = null;
}

/*
   3
  / \
  9 20
    /\
   15 7
     /
     8
*/

const three = new TreeNode(3);
const nine = new TreeNode(9);
const twenty = new TreeNode(20);
const fifteen = new TreeNode(15);
const seven = new TreeNode(7);
const eight = new TreeNode(8);
three.left = nine;
three.right = twenty;
twenty.left = fifteen;
twenty.right = seven;
seven.left = eight;
console.log(maxDepth(three));

const three1 = new TreeNode(3);
const nine1 = new TreeNode(9);
const twenty1 = new TreeNode(20);
const fifteen1 = new TreeNode(15);
const seven1 = new TreeNode(7);
three1.left = nine1;
three1.right = twenty1;
twenty1.left = fifteen1;
twenty1.right = seven1;
console.log(maxDepth2(three1));
