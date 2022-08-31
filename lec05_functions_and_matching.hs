isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (x:xs) = False

isZero :: Int -> Bool
isZero 0 = True
isZero x = False

lucky 3 = "Lucky"
lucky 7 = "So very lucky"
lucky 21 = "The luckiest"
lucky _ = "Unlucky"

addVectors :: (Double, Double) -> (Double, Double)  -> (Double, Double)
addVectors vectA vectB = (fst vectA + fst vectB, snd vectA + snd vectB)

type Vector = (Double, Double)
sumVectors :: Vector -> Vector -> Vector
sumVectors (x1,x2) (y1,y2) = (x1+x2, y1+y2)

addPairs :: [(Int, Int)] -> [Int]
addPairs lst = [a+b | (a,b) <- lst]

addDoubletons :: [[Int]] -> [Int]
addDoubletons lst = [a+b | [a,b] <- lst]

tell :: Show a => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has two elements " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list has many elements, the first two are " ++ show x ++ " and " ++ show y


{-
endsInX (_++"X") = True
endsInX _ = False
-}

--prime (x*y) = x > 1 && y > 1

drop5 [] = []
drop5 lst@(x:xs) = if x == 5 then xs else lst 

firstLetter :: String -> String
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]



