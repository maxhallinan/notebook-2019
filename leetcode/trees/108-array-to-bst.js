class TreeNode {
  constructor(val) {
    this.val = val;
    this.left = null;
    this.right = null;
  }
}

var sortedArrayToBST = (nums) => {
  if (!nums.length) { return null; }
  const rootInd = Math.floor(nums.length / 2)

  const root = new TreeNode(nums[rootInd - 1]);
  let left = rootInd - 1;
  let right = rootInd + 1;
  const insert = (node, tree) => {
    if (node.val < tree.val) {
      tree.left = tree.left ? insert(node, tree.left) : node;
    }
    if (node.val > tree.val) {
      tree.right = tree.right ? insert(node.val, tree.right) : node;
    }
  };
  while (0 <= left || right < nums.length) {
    // console.log(nums, left, right, nums[left], nums[right]);
    if (0 <= left) {
      insert(new TreeNode(nums[left - 1]), root);
    }
    if (right <= nums.length) {
      console.log('hey', nums[right - 1]);
      insert(new TreeNode(nums[right - 1]), root);
    }
    left--;
    right++;
  }
  return root;
};

const treeLevels = (tree) => {
  if (!tree) { return [[]]; }
  const q = [[0, tree]];
  const levels = [];
  while (q.length > 0) {
    const [l, n] = q.shift();
    levels[l] = levels[l] ? levels[l].push(n.val) : [n.val];

    if (n.left) {
      q.push([l + 1, n.left]);
    }

    if (n.right) {
      q.push([l + 1, n.right]);
    }
  }
  return levels;
};

// console.log(treeLevels(sortedArrayToBST([])));
console.log(treeLevels(sortedArrayToBST([1, 2, 3, 4])), sortedArrayToBST([1, 2, 3, 4]));
// console.log(treeLevels(sortedArrayToBST([-10, -7, 0, 4, 9, 100])));
