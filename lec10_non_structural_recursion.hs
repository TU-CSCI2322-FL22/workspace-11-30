import Debug.Trace
{-
 - 1) If list was empty or had one student, it is sorted
 - 2) Take the head of the list of students (call them the pivot)
 - 3) Split the remaining students into younger and older than the pivot
 - 4) Recursively sort the younger and older sublists
 - 5) Combine the younger, pivot, and older in that order
 -}

--quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort lst =
  let pivot   = head lst
      younger = quickSort $ [x | x <- tail lst, x <= pivot]
      older   = quickSort $ [x | x <- lst, x > pivot]
  in younger ++ [pivot] ++ older

egcd a b
  | (a == b)  = a
  | (a > b)   = traceShow (a-b,b) $ egcd (a-b) b
  | otherwise = traceShow (a,b-a) $ egcd a (b-a)

hanoi 0 s m t = []
hanoi n s m t =
  let beginningHalf = hanoi (n-1) s t m 
      middleStep = [(n, s, t)]
      backHalf = hanoi (n-1) m s t
  in beginningHalf ++ middleStep ++ backHalf 
