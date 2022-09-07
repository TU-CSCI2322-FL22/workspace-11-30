import Data.List (nub)
union lstA lstB = nub (lstA ++ lstB)
intersect lstA lstB = [x | x <- lstA, x `elem` lstB]
subset lstA lstB = lstA == (lstA `intersect` lstB)
subset2 lstA lstB = null [x | x <- lstA, x `notElem` lstB]

--powerset lstA = [lst | lst `subset` lstA]
powerset lstA = [[]] ++ [ [x] | x <- lstA] ++ [ [x,y] | x <- lstA, y <- lstA, x < y] 
                ++ [ [x,y,z] | x<-lstA, y<-lstA, z<-lstA, x<y, y<z] ++ [lstA]

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + (len xs)

numNonZero :: [Int] -> Int
numNonZero [] = 0
numNonZero (0:xs) = numNonZero xs
numNonZero (x:xs) = 1 + numNonZero xs

prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * (prod xs)

prodPositive :: (Num a, Ord a) => [a] -> a
prodPositive [] = 1
prodPositive (x:xs) = 
  let ppXs = prodPositive xs
  in if x > 0 then x*ppXs else ppXs

biggest :: Ord a => [a] -> a
biggest [] = 0
biggest (x:xs) =
  let biggestXs = biggest xs
  in max x biggestXs
