import Debug.Trace
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + (mySum xs)
mySum2 lst = katamari (+) 0 lst

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs
myProduct2 lst = katamari (*) 1 lst

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)
myLength2 lst = katamari (\x acc -> 1+acc) 0 lst

combineNumbers :: [Int] -> String
combineNumbers [] = ""
combineNumbers (x:xs) = (show x) ++ "," ++  (combineNumbers xs)
combineNumbers2 nums = foldr (\x acc -> (show x) ++ "," ++ acc) "" nums
combineNumbers3 :: [Int] -> String
combineNumbers3 nums = foldl (\acc x -> (show x) ++ "," ++ acc) "" nums

myConcat :: [String] -> String
myConcat [] = ""
myConcat (s:strs) = s++(myConcat strs)
myConcat2 strs = katamari (++) "" strs

--foo :: [Int] -> Int
katamari :: (a -> b -> b) -> b -> [a] -> b
katamari f b [] = b
katamari f b (x:xs) = x `f` (katamari f b xs)

iramatak :: (b -> a -> b) -> b -> [a] -> b
iramatak f b [] = b
iramatak f b (x:xs) = iramatak f (b `f` x) xs
