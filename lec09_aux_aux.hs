gap :: [Int] -> Int
gap [] = error "Mind the gap"
gap (x:xs) =  aux x x xs
  where aux small big [] = big - small
        aux small big (x:xs) 
          | x < small = aux x big xs
          | x > big   = aux small x xs
          | otherwise = aux small big xs
--gap lst = maximum lst - minimum lst

gap2 :: [Int] -> Int
gap2 lst = 
  let (big, small) = aux lst
  in big - small
  where updateRange x (bigXs, smallXs) 
          | x < smallXs = (bigXs, x)
          | x > bigXs   = (x, smallXs)
          | otherwise   = (bigXs, smallXs)
        aux :: [Int] -> (Int, Int) -- the largest and smallest, tupled
        aux [] = error "Mind the gap2"
        aux [x] = (x,x)
        aux (x:xs) = updateRange x (aux xs)

gap3 :: [Int] -> Int
gap3 lst = 
  let (big, small) = aux lst
  in big - small
  where aux :: [Int] -> (Int, Int) -- the largest and smallest, tupled
        aux [] = error "Mind the gap2"
        aux [x] = (x,x)
        aux (x:xs) = 
          let (bigXs, smallXs) = aux xs
          in (max x bigXs, min x smallXs)

range :: [Int] -> (Int, Int)
range [] = error "Mind the gap2"
range [x] = (x,x)
range (x:xs)  
    | x < smallXs = (bigXs, x)
    | x > bigXs   = (x, smallXs)
    | otherwise   = (bigXs, smallXs)
  where (bigXs, smallXs) = range xs
             
hoursToSchedule :: [(Int, Int)] -> Int
hoursToSchedule tasks = fst $ aux tasks
 where aux ::  [(Int, Int)] -> (Int,Int) --hours required, and minutes of slack
       aux [] = (0,0)
       aux ((h,m):ts) =
          let (hoursForTs, minutesLeft) = aux ts
              newHours = hoursForTs + h + if m > minutesLeft then 1 else 0
              newMinutesLeft = if m < minutesLeft then minutesLeft - m else minutesLeft - m + 60
          in (newHours, newMinutesLeft) 
