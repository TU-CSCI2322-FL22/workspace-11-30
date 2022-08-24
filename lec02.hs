lst = [7,3,2]
lst2 = 8:lst
alphabet = ['a'..'z']
allToHundred = [1..100]
oddsToHundred = [1,3..100]
naturals = [1..]

divisibles = [x | x <- allToHundred, x `mod` 3 == 0, x `mod` 11 == 0]

nums = [-20,7,5,13,8,-1]
someNums = [if x < 0 then x*(-1) else x | x <- nums]
absLst lst = [if x < 0 then x*(-1) else x | x <- lst]


isEmpty [] = True
isEmpty (x:xs) = False
