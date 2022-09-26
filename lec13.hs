
evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) = 
  if (x `mod` 2) == 0
  then x:(evens xs) 
  else evens xs

lowers :: String -> String
lowers "" = ""
lowers (c:cs) = 
  if c `elem` ' ':['a'..'z']
  then c:(lowers cs)
  else lowers cs

collect :: (a -> Bool) -> [a] -> [a]
collect p [] = []
collect p (x:xs) = 
  if p x
  then x:(collect p xs)
  else collect p xs

evens2 lst = filter p lst
  where p x = x `mod` 2 == 0

add7 x = x + const
  where const = 7

evens3 lst = filter (\x -> x `mod` 2 ==0) lst

singletons lst = map (\x -> [x]) lst

scores = [10, 45, 13, 92, 42, 19] 
names = ["Jack", "Jane", "Joe", "Josh", "Jesus", "Jay"]
