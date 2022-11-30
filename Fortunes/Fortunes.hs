module Fortunes where
import Data.Char
import System.IO
import Text.Read (readMaybe)
import System.Environment
import System.Console.GetOpt

data Flag = Help | Quick | Number String deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['q'] ["quick", "quiet"] (NoArg Quick) "Print the fortunes and exit." 
          , Option ['n'] ["num"] (ReqArg Number "<num>" ) "Print <num> fortunes at a time." 
          ]


main :: IO ()
main = do
  args <- getArgs
  let (flags, inputs, error) = getOpt Permute options args
  putStrLn $ show (flags, inputs, error)
  let num = getNumber flags
  if Help `elem` flags
  then printHelp
  else 
    do let fname = if null inputs then "fortunes.txt" else head inputs
       contents <- readFile fname
       let fortunes = lines contents
       if Quick `elem` flags
       then tellFortuneQuick fortunes num
       else interactiveFortunes fortunes 
       

getNumber :: [Flag] -> Int
getNumber ((Number x):_) = 
  case readMaybe x of
    Nothing -> error "That's not a number. Try again." 
    Just n -> n
getNumber (_:flags) = getNumber flags
getNumber [] = 1

tellFortuneQuick :: [String] -> Int -> IO ()
tellFortuneQuick fortunes x =  
  let xFortunes = take x $ drop 6 fortunes
  in putStrLn (unlines xFortunes)


interactiveFortunes :: [String] -> IO ()
interactiveFortunes fortunes = 
 do name <- prompt "What is your name"
    let index = nameToInt name `mod` length fortunes
    putStrLn $ "Hello " ++ name ++ "!"
    tellFortune fortunes index

printHelp :: IO ()
printHelp = putStrLn $ usageInfo "Fortunes [option] [file]" options


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
  hFlush stdout
  response <- getLine
  return response

nameToInt :: String -> Int
nameToInt name = sum [ord c | c <- name] 
