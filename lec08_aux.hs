import Debug.Trace

biggest :: (Ord a, Show a) => [a] -> a
biggest [] = error "There is no biggest element."
biggest [x] = traceShowId x
biggest (x:xs) =
  let biggestXs = biggest xs
  in traceShow (x, xs) ( max x biggestXs)

biggest2 lst =
  if null lst then error "ther eis no biggest element."
  else if length lst == 1 then head lst
  else let x = head lst
           xs = tail lst
           biggestXs = biggest2 xs
       in max x biggestXs

end :: [a] -> a
end = undefined

count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) = 
  let countInYs = count x ys
  in if x == y then 1+countInYs else countInYs
--count 'e' "hello there"
--3

occurancesOfHead :: Eq a => [a] -> Int
occurancesOfHead [] = error "There is no head of the empty list to occur, you occurer."
occurancesOfHead (x:xs) = aux x xs -- 1 + count x xs
  where aux x [] = 1
        aux x (y:ys) = 
          let auxInYs = aux x ys
          in if x == y then 1+auxInYs else auxInYs

occurancesOfHead2 :: Eq a => [a] -> Int
occurancesOfHead2 [] = error "There is no head of the empty list to occur, you occurer."
occurancesOfHead2 (x:xs) = aux xs -- 1 + count x (x:xs) 
  where aux [] = 1
        aux (y:ys) = 
          let auxInYs = aux ys
          in if x == y then 1+auxInYs else auxInYs

--rev :: [a] -> [a]
rev lst = aux [] lst
  where aux acc [] = acc
        aux acc (x:xs) = aux (x:acc) xs
--rev "hello"
--"olleh"
