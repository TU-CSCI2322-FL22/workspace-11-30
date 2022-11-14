module Fortunes where
import Data.Char

main :: IO ()
main = do
  contents <- readFile "fortunes.txt"
  let fortunes = lines contents
  name <- prompt "What is your name"
  let index = nameToInt name `mod` length fortunes
  putStrLn $ "Hello " ++ name ++ "!"
  tellFortune fortunes index


tellFortune :: [String] -> Int -> IO ()
tellFortune fortunes index = do
  putStr "Your fortune is: "
  putStrLn (fortunes !! index)
  answer <- prompt "Do you want another"
  if map toLower answer `elem` ["yes", "y", "sure", "ok", "yeah", "si"]
  then tellFortune fortunes (index+1)
  else return ()

prompt :: String -> IO String
prompt question = do
  putStr $ question ++ ": "
  response <- getLine
  return response

nameToInt :: String -> Int
nameToInt name = sum [ord c | c <- name] 
