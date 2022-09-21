import Data.List

sorted :: Ord a => [a] -> Bool
--sorted lst = (list == sort lst)
sorted [] = True
sorted (x:lst) = aux x lst
  where aux x [] = True
        aux x (y:xs) = (x < y) && (aux y xs)
--sorted should return true if the list is sorted smallest to largest

rle :: (Eq a, Ord a) => [a] -> [(a, Int)]
rle [] = []
rle (x:xs) = 
  let ((currChar, currCount):rleXs) = rle xs
  in if x == currChar 
     then (x,currCount+1):rleXs
     else (x,1):(currChar, currCount):rleXs

rle2 :: (Eq a, Ord a) => [a] -> [(a, Int)]
rle2 [] = []
rle2 word = 
    let (currChar, currCount, rleWord) = aux word
    in (currChar, currCount):rleWord
  where aux :: Eq a => [a] -> (a, Int, [(a, Int)])
        aux [x] = (x, 1, [])
        aux (x:xs) = 
          let (currentChar, currentCount, rleXs) = aux xs
          in if x == currentChar
             then (x, currentCount+1,rleXs)
             else (x, 1, (currentChar, currentCount):rleXs)
--run length encoding turns a word into tuples of runs of a letter
--rle "aaabbbbcca" = [('a',3), ('b', 4), ('c', 2), ('a', 1)]
