import Data.Maybe

lookupVal :: Eq k => k -> [(k,v)] -> Maybe v
lookupVal key assocs = 
  let results = [v | (k,v) <- assocs, k == key]
  in if not $ null results
     then Just (head results)
     else Nothing

charsToNums = zip (['a'..'z']++['A'..'Z']) ([1..26]++[1..26])

sumWord [] = 0
sumWord (c:cs) = 
  case lookupVal c charsToNums of
    Just x   -> x + sumWord cs
    Nothing  -> 1+ sumWord cs

defaultZero :: Maybe Int -> Int
defaultZero Nothing = 0
defaultZero (Just x)= x

defaultX :: a -> Maybe a -> a
defaultX x Nothing = x
defaultX x (Just y)= y

sumWord2 [] = 0
sumWord2 (c:cs) = (defaultZero $ lookupVal c charsToNums) + sumWord cs
