99 Haskell Problems
===================

1. Find the last element of a list.
2. Find the last but one element of a list.
3. Find the K'th element of a list. The first element in the list is number 1.
4. Find the number of elements of a list.
5. Reverse a list.
6. Find out whether a list is a palindrome. A palindrome can be read forward or backward.
7. Flatten a nested list structure.
8. Eliminate consecutive duplicates of list elements.
9. Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
10. Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
11. Modified run-length encoding. Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
12. Decode a run-length encoded list. Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
13. Run-length encoding of a list (direct solution). Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
14. Duplicate the elements of a list.
15. Replicate the elements in a list a given number of times.
16. Drop every N'th element from a list.
17. Split a list into two parts; the length of the first part is given. Do not use any defined predicates.
18. Extract a slice from a list. Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
19. Rotate a list N places to the left.
20. Remove the K'th element from a list.
21. Insert an element at a given position into a list.
22. Create a list containing all integers within a given range.
23. Extract a given number of randomly selected elements from a list.
24. Lotto: Draw N different random numbers from the set 1..M.
25. Generate a random permutation of the elements of a list.
26. Generate the combinations of K distinct objects chosen from the N elements of a list. In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
27. Group the elements of a set into disjoint subsets.
28. Sorting a list of lists according to length of sublists.
31. Determine whether a given integer number is prime.
32. Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
33. Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
34. Calculate Euler's totient function phi(m). Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m. 
35. Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
36. Determine the prime factors of a given positive integer. Construct a list containing the prime factors and their multiplicity.
37. Calculate Euler's totient function phi(m) (improved).
39. A list of prime numbers. Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
40. Goldbach's conjecture. Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
41. Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition. In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
46. Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed. A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)). Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
47. Truth tables for logical expressions (2). Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.
48. Truth tables for logical expressions (3). Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.
49. Gray codes. An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. Find out the construction rules and write a predicate with the following specification.
50. Huffman codes. We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.].
55. Construct completely balanced binary trees. In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one. Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
56. Symmetric binary trees. Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
57. Binary search trees (dictionaries). Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.
58. Generate-and-test paradigm. Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
59. Construct height-balanced binary trees. In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.
60. Construct height-balanced binary trees with a given number of nodes. Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?
61. Count the leaves of a binary tree.
61A. Collect the leaves of a binary tree in a list.
62. Collect the internal nodes of a binary tree in a list.
62B. Collect the nodes at a given level in a list.
63. Construct a complete binary tree.
64. Given a binary tree, write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or the rectangle bounding the drawn tree.
65. Find out the rules and write the corresponding function. Hint: On a given level, the horizontal distance between neighboring nodes is constant.
66. Find out the rules and write the corresponding function. Hint: Consider the horizontal distance between a node and its successor nodes. How tight can you pack together two subtrees to construct the combined binary tree?
67A. Write a predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form.
68. Write predicates preorder/2 and inorder/2 that construct the preorder and inorder sequence of a given binary tree, respectively. Can you use preorder/2 from problem part a) in the reverse direction; i.e. given a preorder sequence, construct a corresponding tree? If not, make the necessary arrangements. If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given, then the tree is determined unambiguously. Write a predicate pre_in_tree/3 that does the job.
69. Dotstring representation of binary trees. We consider again binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67. Such a tree can be represented by the preorder sequence of its nodes in which dots (.) are inserted where an empty subtree (nil) is encountered during the tree traversal. For example, the tree shown in problem P67 is represented as 'abd..e..c.fg...'. First, try to establish a syntax (BNF or syntax diagrams) and then write a predicate tree_dotstring/2 which does the conversion in both directions. Use difference lists.
70C. Count the nodes of a multiway tree.
70. Define the syntax of the string and write a predicate tree(String,Tree) to construct the Tree when the String is given. Make your predicate work in both directions.
71. Determine the internal path length of a tree. We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree. By this definition, tree5 has an internal path length of 9.
72. Construct the bottom-up order sequence of the tree nodes.
73. Lisp-like tree representation. Write a predicate tree_ltl(T,LTL) which constructs the "lispy token list" LTL if the tree is given as term T in the usual Prolog notation. As a second, even more interesting exercise try to rewrite tree_ltl/2 in a way that the inverse conversion is also possible.
80. Write predicates to convert between the different graph representations. With these predicates, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form.
81. Path from one node to another one. Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b.
