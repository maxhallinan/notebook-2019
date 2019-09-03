/*
 * gIven a binary tree, determine if it is a valid binary search tree (BST).
 * Assume a BST is defined as follows:
 * - The left subtree of a node contains only nodes with keys *less than* the 
 *   node's key.
 * - The right subtree of a node contains only nodes with keys *greater than* 
 *   the node's key.
 * - Both the left and right subtrees must also be binary search trees.
 */
const isValidBST = (root) => {
  const lefts = [];
  const rights = [];
  // const go = (node, isSideValid) => {
  //   if (!node) {
  //     return true;
  //   }
  //   const isSide = isSideValid(node);
  //   const isLeftLTNode = !node.left || (node.left.val < node.val);
  //   const isLeftSubValid = go(node.left, (n) => (n.val < node.val) && isSideValid(n));
  //   const isRightGTNode = !node.right || (node.val < node.right.val);
  //   const isRightSubValid = go(node.right, (n) => (n.val > node.val) && isSideValid(n));
  //   return isSide && isLeftLTNode && isLeftSubValid && isRightGTNode && isRightSubValid;
  // };
  // return go(root.left, (n) => n.val < root.val) &&
  //   go(root.right, (n) => n.val > root.val) &&
  //   (root.left && (root.left.val < root.val)) && (root.right && (root.right.val > root.val));
};
// const isValidBST = function (root) {
//   const go = (node) => {
//     // console.log(node ? node.val : node, node.left ? node.left.val : null, node.right ? node.right.val : null);
//     if (!node) {
//       return true;
//     }
    
//     const isLeftValid =
//       (!node.left || node.left.val < node.val) &&
//       (node === root || node.val < root.val);

//     const isLeftSubValid = go(node.left);

//     const isRightValid = 
//       (!node.right || node.val < node.right.val) &&
//       (node === root || root.val < node.val);

//     const isRightSubValid = go(node.right);

//     console.log(isLeftValid, isLeftSubValid, isRightValid, isRightSubValid);
//     return isLeftValid && isRightValid;
//   };
//   return go(root);
// };

const TreeNode = function (val) {
  this.val = val;
  this.left = null;
  this.right = null;
};

const one = new TreeNode(1);
const two = new TreeNode(2);
const three = new TreeNode(3);
two.left = one;
two.right = three;
console.log(isValidBST(two));

const ten = new TreeNode(10);
const five = new TreeNode(5);
const fifteen = new TreeNode(15);
const six = new TreeNode(6);
const twenty = new TreeNode(20);
ten.left = five;
ten.right = fifteen;
fifteen.left = six;
fifteen.right = twenty;
console.log(isValidBST(ten));
