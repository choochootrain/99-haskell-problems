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
