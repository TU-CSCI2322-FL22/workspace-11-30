--Higher order functions and their types
{- ● foldl :: (b -> a -> b) -> b -> [a] -> b
-  ● foldr :: (a -> b -> b) -> b -> [a] -> b
-  ● map :: (a -> b) -> [a] -> [b]
-  ● filter :: (a -> Bool) -> [a] -> [a]
-}

-- 1. Define addPairs :: [(Int, Int)] -> [Int] to add the elements of each pair in a list.

addPairs lst = map (\elem -> fst elem + snd elem) lst
addPairs2 lst = map (\(a,b) -> a + b) lst
addPairs3 lst = map mapFunc lst
  where mapFunc (a,b) = a + b

-- 2. Define prodList :: [Int] -> Int that finds the product of all elements.

prodList :: [Int] -> Int
prodList lst = foldr (\x recVal -> x * recVal) 1 lst

-- 3. Define zeroDetector :: [Int] -> Bool to check if there is a zero in a list
  -- a. (Optional) Using a filter and built-in functions

  -- b. Using a fold

-- 4. Using a fold, define positives :: [Int] -> [Int] that returns the positive elements.

-- 5. Using a fold, define range :: [Int] -> (Int,Int)

-- 6. Define splitOnParity :: [Int] -> ([Int], [Int]) to split a list into evens and odds.
  -- a. Using two filters
  -- b. Using one fold

-- 7. Define a higher-order function exists :: (a -> Bool) -> [a] -> Bool that takes a predicate, a list, and returns true if the predicate is true for anything in the list.
  -- a. Using recursion.
  -- b. (Optional) using a fold.

-- 8. Rewrite zeroDetector using only exists.

-- 9. Define sumValues :: [(a, Int)] -> Int that sums the values of an association list.

-- 10. Define bestKey :: [(a, Int)] -> a that finds the value with the best key.

-- 11. Using exists, define isPrime :: Int -> Bool
  -- a. First define isMultiple :: Int -> Int -> Bool that takes two Ints and returns True if the first can be divided by the second.