module Fortunes where
import Data.Char

main :: IO ()
main = do
  contents <- readFile "fortunes.txt"
  let fortunes = lines contents
  putStr "What is your name: "
  name <- getLine
  putStrLn $ "Hello " ++ name ++ ", your fortune is:"
  putStrLn (fortunes !! (nameToInt name `mod` length fortunes))

nameToInt :: String -> Int
nameToInt name = sum [ord c | c <- name] 
