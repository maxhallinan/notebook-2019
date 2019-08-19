# Algorithms and Datastructures

## 32-bit signed integer

**What is a 32-bit signed integer**

A 32-bit signed integer is an integer whose value is represented by 32 bits.
That means it's a string of 32 0's and 1's.
"Signed" means that both positive and negative integers can be represented.
One of the bits (e.g. the first bit) is used as the sign.
That leaves 31 bits for the number itself.

The smallest value in 31 bits is -(2^31): -2147483648.
The largest value in 31 bits is (2^31) - 1: 2147483647.

## Avoiding nested loops

Sometimes I find that I have to go through a sequence and compare each item in 
a sequence to all of the other items in a sequence.
One example of this is finding the first unique character in a string.
My first impulse is to use nested for-loops.
For each character in the string, loop through the entire string and compare the
two.
This has O(N^2) time complexity.

One technique for avoiding nested for-loops is to use a hash map.
The first step is to go through the string and count the occurrences of each 
character.
Store these counts in a map of character to occurrence count.
This time complexity is O(N).
Then, go through the string again.
Now check if the occurrence count for that character is `1`.
If it is, return the index of the character.
At the end, return `-1`.
This second loop is also O(N).

The space complexity is O(N) instead of O(1) with the nested loops, but the 
time complexity is O(2N) instead of O(N^2).
I think the implementation is also more readable.

## The two-pointer technique

Two pointers:

- One fast pointer
- One slow pointer

Pointer `a` starts at the beginning.
Pointer `b` starts at the end.

```javascript
const twoPointer = (xs) => {
  const a = 0;
  const b = xs.length - 1;

  while (a < b) {
    // ... do something here
    a++;
    b--;
  }
};
```

For example, reverse an array:

```javascript
const reverse = (xs) => {
  const a = 0;
  const b = xs.length - 1;

  while (a < b) {
    let temp = xs[a];
    xs[a] = xs[b];
    xs[b] = temp;  
    a++;
    b--;
  }
}
```
