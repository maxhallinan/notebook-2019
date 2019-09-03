var levelOrder = (root) => {
  if (!root) { return []; }
  const q = [[0, root]];
  const levels = [];
  while (q.length) {
    const [l, n] = q.shift();
    if (!levels[l]) {
      levels[l] = [n.val];
    } else {
      levels[l].push(n.val);
    }
    if (n.left) {
      q.push([l + 1, n.left]);
    }
    if (n.right) {
      q.push([l + 1, n.right]);
    }
  }
  return levels;
};

class TreeNode {
  constructor(val) {
    this.val = val;
    this.left = null;
    this.right = null;
  }
}

const one = new TreeNode(1);
const two = new TreeNode(2);
const three = new TreeNode(3);
const four = new TreeNode(4);
const five = new TreeNode(5);
const six = new TreeNode(6);
const seven = new TreeNode(7);
const eight = new TreeNode(8);
const nine = new TreeNode(9);

one.left = two;
one.right = three;
two.left = four;
two.right = five;
three.left = six;
three.right = seven;
six.left = eight;
six.right = nine;

console.log(levelOrder(one));
