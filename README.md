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
