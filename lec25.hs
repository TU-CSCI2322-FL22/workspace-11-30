main :: IO ()
main = putStrLn "Hello World"

talkToUser :: IO ()
talkToUser = 
  do putStr "What is your name: "
     x <- getLine 
     putStrLn ("Hello there: " ++ x)
