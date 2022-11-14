data Contest = Rock | Scissor | Paper deriving (Show, Read)
--This actually works!
x :: Contest
x = read "Paper"

rps :: Contest -> Contest -> String
rps Rock Scissor = "Player One Wins"
rps Scissor Paper = "Player One Wins"
rps Paper Rock = "Player One Wins"
rps Scissor Rock = "Player Two Wins"
rps Paper Scissor = "Player Two Wins"
rps Rock Paper = "Player Two Wins"
rps signOne signTwo = "Tie"

data Velocity = MPS Double | FPS Double deriving Show

toMPS :: Velocity -> Double
toMPS (MPS x) = x
toMPS (FPS y) = y * 0.3048

type Point = (Double, Double)
data Shape = Circle Point Double 
           | Rectangle Point Point deriving Show

area :: Shape -> Double
area (Circle center r) = pi * r^2
area (Rectangle (x1, y1) (x2, y2)) = abs $ (x2 - x1) * (y2 - y1)

data ITsil = ILlun | ISnoc ITsil Integer deriving Show
data STsil = SLlun | SSnoc STsil String deriving Show
data Tsil a = Llun | Snoc (Tsil a) a deriving Show

daeh :: Tsil a -> Maybe a 
daeh Llun = Nothing
daeh (Snoc xs x) = Just x

tsl = Snoc (Snoc (Snoc Llun 5) 3) 7
tsl2 = Snoc (Snoc (Snoc Llun [7,2]) [3,5,9]) []
tmp = Just []

listToTsil :: [a] -> Tsil a
listToTsil [] = Llun
listToTsil (x:xs) = (listToTsil xs) `Snoc` x

tsilToList :: Tsil a -> [a]
tsilToList Llun = []
tsilToList (xs `Snoc` x) = x:(tsilToList xs)

pam :: (a -> b) -> Tsil a -> Tsil b
--pam f tsl = listToTsil (map f (tsilToList tsl))
pam f Llun = Llun
pam f (xs `Snoc` x) =(pam f xs) `Snoc` (f x)
