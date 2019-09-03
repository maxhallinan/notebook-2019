const levelOrder = (root) => {
  const levels = [[root.val]];
  const leftVal = root.left ? root.left.val : null;
  const rightVal = root.right ? root.right.val : null;

  // const go = (node) => {
  //   const level = [];
  //   if (node.left) {
  //     level.push 
  //   }
  //   if (node.right) {
  //   }
  //   go(node);
  // };
  // go(root);
  return levels;
};

function TreeNode(val) {
  this.val = val;
  this.left = null;
  this.right = null;
}
