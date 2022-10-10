data Contest = Rock | Scissor | Paper deriving Show

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

