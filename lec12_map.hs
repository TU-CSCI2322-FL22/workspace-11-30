



add7, add13, add9, sub2 :: Int -> Int
add7 x = x + 7
add13 x = x + 13
add9 x = x + 9
sub2 x = x - 2

addY y x = x + y

add7b x = addY 7 x 
sub2b x = addY (-2) x

gradedFunction :: Int -> (Int -> Int)
gradedFunction a b = (addY 7 a) `div` (addY 13 b)

addYAll :: Int -> [Int] -> [Int]
addYAll y [] = []
addYAll y (x:xs) = (x+y):(addYAll y xs)

add13All :: [Int] -> [Int]
add13All [] = []
add13All (x:xs) = (x+13):(add13All xs)

absAll :: [Int] -> [Int]
absAll [] = []
absAll (x:xs) = (abs x):(absAll xs)

singletons :: [a] -> [[a]]
singletons [] = []
singletons (x:xs) = [x]:(singletons xs)
singletons2 lst = map single list
  where single x = [x]

applyToAll :: (a -> b) -> [a] -> [b]
applyToAll f [] = []
applyToAll f (x:xs) = (f x):(applyToAll f xs)

cutOff :: [Int] -> [Int]
{-cutOff [] = []
cutOff (x:xs) = (max 0 x):(cutOff xs)-}
cutOff lst = map (max 0) lst
cutOff2 lst = [max 0 x | x <- lst]

shoutNumbers :: [Int] -> [String]
shoutNumbers lst = map (++"!") (map show lst)
shoutNumbers2 lst = [(show x) ++ "!" | x <- lst]
shoutNumbers3 lst = map func lst
  where func x = (show x) ++ "!"

