import Data.List

sorted :: Ord a => [a] -> Bool
--sorted lst = (list == sort lst)
sorted [] = True
sorted (x:lst) = aux x lst
  where aux x [] = True
        aux x (y:xs) = (x < y) && (aux y xs)
--sorted should return true if the list is sorted smallest to largest

rle :: (Eq a, Ord a) => [a] -> [(a, Int)]
--run length encoding turns a word into tuples of runs of a letter
--rle "aaabbbbcca" = [('a',3), ('b', 4), ('c', 2), ('a', 1)]
