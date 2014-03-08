import Data.List
import Data.Ord (comparing)
import System.Random (newStdGen, randomR, randomRs)
import Control.Monad


-- 1. Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "list is empty"
myLast (head : []) = head
myLast (_ : tail) = myLast tail


-- 2. Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "list is too short"
myButLast (x : []) = error "list is too short"
myButLast x = myLast $ init x


-- 3. Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt list x = if x <= length list && x > 0
                      then list !! (x - 1)
                      else error "index out of bounds"


-- 4. Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : x) = 1 + myLength x


-- 5. Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (head : tail) = (myReverse tail) ++ [head]


-- 6. Find out whether a list is a palindrome. A palindrome can be read forward
-- or backward.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome x = head x == last x && (isPalindrome . tail . init) x


-- 7. Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List x) = foldr ((++) . flatten) [] x


-- 8. Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress x = foldr (\x y -> if length y >= 1 && x /= y !! 0 then [x] ++ y else y)[last x] $ init x

-- 9. Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate
-- sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack x = foldr (\x y -> if x /= head y !! 0
                          then [[x]] ++ y
                          else ([(head y ++ [x])] ++ tail y))
               [[last x]] $ init x


-- 10. Run-length encoding of a list. Use the result of problem P09 to implement
-- the so-called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = (map (\x -> (length x, head x))) . pack


-- 11. Modified run-length encoding. Modify the result of problem 10 in such a
-- way that if an element has no duplicates it is simply copied into the result
-- list. Only elements with duplicates are transferred as (N E) lists.
data Encoding a = Single a
                | Multiple Int a
                deriving (Show)
encodeElement :: [a] -> Encoding a
encodeElement (x:[]) = Single x
encodeElement x = Multiple (length x) (head x)
encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified = (map encodeElement) . pack


-- 12. Decode a run-length encoded list. Given a run-length code list generated
-- as specified in problem 11. Construct its uncompressed version.
decodeElement :: Encoding a -> [a]
decodeElement (Single x) = [x]
decodeElement (Multiple n x) = replicate n x
decodeModified :: [Encoding a] -> [a]
decodeModified = concatMap decodeElement

-- 13. Run-length encoding of a list (direct solution). Implement the so-called
-- run-length encoding data compression method directly. I.e. don't explicitly
-- create the sublists containing the duplicates, as in problem 9, but only
-- count them. As in problem P11, simplify the result list by replacing the
-- singleton lists (1 X) by X.
encodeDirect :: (Eq a) => [a] -> [Encoding a]
encodeDirect = foldr encodeElementDirect []
  where
    encodeElementDirect x [] = [Single x]
    encodeElementDirect x ((Single y):ys) = if x == y
                                            then ((Multiple 2 y):ys)
                                            else [Single x] ++ ((Single y):ys)
    encodeElementDirect x ((Multiple n y):ys) = if x == y
                                                then ((Multiple (n+1) y):ys)
                                                else [Single x] ++ ((Multiple n y):ys)


-- 14. Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = concatMap $ take 2 . repeat


-- 15. Replicate the elements in a list a given number of times.
repli :: [a] -> Int -> [a]
repli x n = concatMap (replicate n) x


-- 16. Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery x n
  | length x >= n = (take (n-1) x) ++ (dropEvery (drop n x) n)
  | otherwise     = x


-- 17. Split a list into two parts; the length of the first part is given. Do not use any defined predicates.
split :: [a] -> Int -> ([a], [a])
split x n = tupleMap fst (filter before xi, filter after xi)
  where
    tupleMap f (t1, t2) = (map f t1, map f t2)
    before = ((<n) . snd)
    after = ((>=n) . snd)
    xi = zip x [0..length x]


-- 18. Extract a slice from a list. Given two indices, i and k, the slice is the
-- list containing the elements between the i'th and k'th element of the original
-- list (both limits included). Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice x i j = snd $ split (fst $ split x $ j) $ i-1


-- 19. Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate x n
  | n >= 0    = drop n x ++ take n x
  | otherwise = rotate x (length x + n)


-- 20. Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt i x = (x !! (i-1), map fst $ filter ((/=(i-1)) . snd) $ zip x [0..length x])


-- 21. Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x y n = before ++ [x] ++ after
  where
    chunks = split y (n-1)
    before = fst chunks
    after  = snd chunks


-- 22. Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range x y = [x..y]


-- 23. Extract a given number of randomly selected elements from a list.
rndSelect :: [a] -> Int -> IO [a]
rndSelect x n = do
  gen <- newStdGen
  return $ map (\i -> x !! i) $ take n $ randomRs (0, length x - 1) gen


-- 24. Lotto: Draw N different random numbers from the set 1..M.
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  gen <- newStdGen
  return $ take n $ nub $ randomRs (1, m) gen


-- 25. Generate a random permutation of the elements of a list.
rndPermutation :: [a] -> IO [a]
rndPermutation x = do
  gen <- newStdGen
  return $ (permutations x) !! (fst $ randomR (0, product [1..length x] - 1) gen)


-- 26. Generate the combinations of K distinct objects chosen from the N
-- elements of a list. In how many ways can a committee of 3 be chosen from a
-- group of 12 people? We all know that there are C(12,3) = 220 possibilities
-- (C(N,K) denotes the well-known binomial coefficients). For pure
-- mathematicians, this result may be great. But we want to really generate
-- all the possibilities in a list.
combinations :: Int -> [a] -> [[a]]
combinations n x
  | n == 1        = map (\y -> [y]) x
  | length x == n = [x]
  | otherwise     = firsts ++ others
    where
      firsts = map (\y -> [head x] ++ y) $ combinations (n-1) $ tail x
      others = combinations n $ tail x


-- 27. Group the elements of a set into disjoint subsets.
disjointGroups :: (Eq a) => [Int] -> [a] -> [[[a]]]
disjointGroups [] _ = [[]]
disjointGroups (n:ns) x = concatMap createGroup groups
  where
    splitGroups :: (Eq a) => Int -> [a] -> [([a], [a])]
    splitGroups n x = zip combs $ map (\y -> x \\ y) combs
      where
        combs = combinations n x
    groups = splitGroups n x
    createGroup :: (Eq a) => ([a], [a]) -> [[[a]]]
    createGroup (x, y) = map (\z -> [x] ++ z) $ disjointGroups ns y


-- 28. Sorting a list of lists according to length of sublists.
lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)
  where
    compareLengths x y = compare (length x) (length y)
lfsort :: [[a]] -> [[a]]
lfsort x = map ((x !!) . fst) pairs
  where
    lengthFrequencies = map (occurrences x) $ map length x
      where
        occurrences x n = length $ filter (\y -> y == n) $ map length x
    pairs = sortBy (comparing snd) $ zip [0..length x] lengthFrequencies


-- 31. Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime n = numFactors == 2
  where
    numFactors = length $ filter (\x -> (n`mod`x == 0)) [1..n]


-- 32. Determine the greatest common divisor of two positive integer numbers.
-- Use Euclid's algorithm.
myGCD :: Int -> Int -> Int
myGCD n m = euclidsAlgorithm max min
  where
    max = maximum [n, m]
    min = minimum [n, m]
    euclidsAlgorithm a b
      | b == 0    = abs a
      | otherwise = euclidsAlgorithm b $ a `rem` b
