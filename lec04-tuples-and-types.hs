rect = [(0,0), (0,1), (1,1), (1,0)]

schedule = ("Jane McCrazyOverload", ["CSCI 2322", "CSCI 2320", "Math 3326", "BIO 3446", "CSCI3320"])

aTuple = ("Jacob", 19)
x = 7.2
q = 7
b = x + q

aList = [[5,9], [], [7]]
anotherTuple = (7,3,9)
extarFunList = reverse [1..]

removeUpper :: String -> String
removeUpper str = [x | x <- str, not (x `elem` ['A'..'Z']) ]

y = 2
z = 5
w = y / z
addThree :: Integer -> Integer -> Integer -> Integer
addThree a b c = a + b + c

crossProduct :: [a] -> [b] -> [(a, b)]
crossProduct xLst yLst = [(x,y) | x <- xLst, y <- yLst]

makeList :: a -> a -> [a]
makeList x y = [x,y]




