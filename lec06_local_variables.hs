myEven :: Integer -> Bool
myEven x | (x `mod` 2 == 0) = True
         | otherwise        = False
-- of course this could just be
-- myEven x = (x `mod` 2 == 0) 

mileage :: Double -> String
mileage mpg 
  | (mpg <= 10.0) = "Get a new car."
  | (mpg <= 20.0) = "You're doing okay."
  | (mpg == 25)   = "Excellent"
  | (mpg <= 30.0) = "You eco-warrior, you."
  | otherwise     = "You're lying, aren't you?"

oldmileage :: (Double, Double) -> String
oldmileage (miles, gallons)
    | (mpg <= 10.0) = "Get a new car."
    | (mpg <= 20.0) = "You're doing okay."
    | (mpg == 25)   = "Excellent"
    | (mpg <= 30.0) = "You eco-warrior, you."
    | otherwise     = "You're lying, aren't you?"
  where mpg = miles / gallons
        gpm = gallons / miles


--wheres cannot be shared across different cases
first = "The first!"
lexCmp :: Ord a => [a] -> [a] -> String
lexCmp (x:xs) (y:ys)
  | x < y     = first
  | x > y     = "The second!"
  | otherwise = "Who knows!"
lexCmp [] (y:ys) = first
lexCmp (x:xs) [] = "The second!"
lexCmp [] [] = "Who knows!"

circleArea r = pi*r^2
circleCircum r = 2*pi*r

cylinderArea2 r h = 
  in 2*circleArea r + h*(circleCircum r)

cylinderArea r h = 
  let topArea = pi*r^2 
      sideArea = 2*pi*r * h
  in 2*topArea + sideArea

sOfL :: [String] -> Int -> [String]
sOfL strs len = [str | str <- strs, length str == len]

stringsOfLengths :: [String] -> [Int] -> [(Int, [String])]
stringsOfLengths strs lens = [(len, lenStrs) | len <- lens, let lenStrs = sOfL strs len]

stringsOfLengths2 :: [String] -> [Int] -> [(Int, [String])]
stringsOfLengths2 strs lens = [(len, sOfL len) | len <- lens]
  where sOfL ::  Int -> [String]
        sOfL len = [str | str <- strs, length str == len]
