const isSymmetric = (root) => {
  if (!root) {
    return true;
  }
  const q = [[0,root]];
  const levels = [];

  while (q.length) {
    const [l, n] = q.shift();
    const val = !!n ? n.val : null;
    if (levels[l]) {
      levels[l].push(val);
    } else {
      levels[l] = [val];
    }
    if (!!n) {
      q.push([l + 1, n.left]);
      q.push([l + 1, n.right]);
    }
  }

  for (var i = 0; i < levels.length; i++) {
    for (var j = 1; j <= levels[i].length; j++) {
      const length = levels[i].length;
      const x = levels[i][j - 1];
      const y = levels[i][length - j];
      if (x !== y) {
        return false;
      }
    }
  }
  return true;
};

const isSymmetric = (root) => {
  const q = [root, root];
  while(q.length > 0) {
  }
};

const TreeNode = function (val) {
  this.val = val;
  this.left = null;
  this.right = null;
};

const one = new TreeNode(1);
const two = new TreeNode(2);
const three = new TreeNode(3);
const four = new TreeNode(4);
const five = new TreeNode(5);
const six = new TreeNode(6);
const seven = new TreeNode(7);
one.left = two;
one.right = three;

two.left = four;
two.right = five;

three.left = six;
three.right = seven;
console.log(isSymmetric(one));

const l1 = new TreeNode(1);

const l2a = new TreeNode(2);
const l2b = new TreeNode(2);

l1.left = l2a;
l1.right = l2b;

const l3a = new TreeNode(3);
const l3b = new TreeNode(4);
l2a.left = l3a;
l2a.right = l3b;

const l3c = new TreeNode(4);
const l3d = new TreeNode(3);
l2b.left = l3c;
l2b.right = l3d;
console.log(isSymmetric(l1));

const a1 = new TreeNode(1);
const a2a = new TreeNode(2);
const a2b = new TreeNode(2);
const a3a = new TreeNode(3);
const a3b = new TreeNode(3);
a1.left = a2a;
a1.right = a2b;
a2a.right = a3a;
a2b.left = a3b;
console.log(isSymmetric(a1));

const c1 = new TreeNode(1);
const c2a = new TreeNode(2);
const c2b = new TreeNode(2);
const c3a = new TreeNode(3);
const c3b = new TreeNode(3);
c1.left = c2a;
c1.right = c2b;
c2a.right = c3a;
c3b.right = c3b;
console.log(isSymmetric(c1));
