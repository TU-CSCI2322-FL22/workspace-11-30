import Debug.Trace
foo [] = 0
foo (x:xs) = let fooXs = foo xs in (fooXs) + (2 + fooXs)

uniques [] = []
uniques (x:xs) = 
  if x `elem` xs
  then uniques xs
  else x:(uniques xs)
