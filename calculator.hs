import Debug.Trace
import Text.Read
---- cannot inspect values
--data Token = Number Double | Oper (Double -> Double -> Double) deriving Show  
---- lacks fidelity
--data Token = Number Double | Oper Char deriving Show

--Valid but we voted against it
--data Token = NumberTok Double | PlusTok | MinusTok | TimesTok | DivTok deriving Show
--data Expr = NumE Double | PlusE Expr Expr | MinusE Expr Expr | TimesE Expr Expr | DivE Expr Expr deriving (Eq, Show)

data Token = NumTok Double | OpTok Operator deriving (Show, Eq)
data Operator = Plus | Minus | Times | Div deriving (Show, Eq)

inputOne = "- + 79 4 * 8 2"
inputTwo = "* + 3 - 79 8 2"

lexer :: String -> Maybe [Token]
lexer str = seqMaybe $ map lexWord (words str)

seqMaybe :: [Maybe a] -> Maybe [a]
seqMaybe (x:xs) = 
  case (x, seqMaybe xs) of
    (Just v, Just xsvals) -> Just (v:xsvals)
    _ -> Nothing
seqMaybe [] = Just []

lexWord :: String -> Maybe Token
lexWord "+" = Just $ OpTok Plus
lexWord "-" = Just $ OpTok Minus
lexWord "/" = Just $ OpTok Div
lexWord "*" = Just $ OpTok Times
lexWord str = applyToMaybe NumTok (readMaybe str)

--built in as fmap
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe f Nothing = Nothing
applyToMaybe f (Just x) = Just $ f x

tokensA = [NumTok 79]
tokensB = [OpTok Plus, NumTok 79, NumTok 4]
tokensW = [OpTok Minus, NumTok 4, OpTok Times, NumTok 8, NumTok 2]
tokensV = [OpTok Minus, OpTok Times, NumTok 8, NumTok 2, NumTok 4]
tokensOne = [OpTok Minus, OpTok Plus, NumTok 79, NumTok 4, OpTok Times, NumTok 8, NumTok 2]

sizeExpr :: Expr -> Int
sizeExpr (NumE _) = 1
sizeExpr (OpE op lt rt) = 1 + sizeExpr lt + sizeExpr rt

data Expr = NumE Double | OpE Operator Expr Expr deriving (Show, Eq)

parseCrash :: [Token] -> Expr
parseCrash toks = 
  case aux toks of
    (expr, []) -> expr
    (expr, toks) -> error $ "Too many tokens: " ++ show toks
  where aux :: [Token] -> (Expr, [Token])
        aux []     = error "No tokens to parse"
        aux (NumTok x:ts) = (NumE x, ts)
        aux (OpTok op:ts) = 
          let (lt, afterLeft) = aux ts
              (rt, afterRight) =  aux afterLeft
          in (OpE op lt rt, afterRight)

parse :: [Token] -> Maybe Expr
parse toks = 
  case aux toks of
    Nothing -> Nothing
    Just (expr, []) -> Just expr
    Just (expr, toks) -> Nothing -- error $ "Too many tokens: " ++ show toks
  where aux :: [Token] -> Maybe (Expr, [Token])
        aux []     = Nothing -- error "No tokens to parse"
        aux (NumTok x:ts) = Just (NumE x, ts)
        aux (OpTok op:ts) = 
          case aux ts of
            Nothing -> Nothing
            Just (lt, afterLeft) -> 
              case aux afterLeft of
                Nothing -> Nothing
                Just (rt, afterRight) -> Just (OpE op lt rt, afterRight)


parseOld []     = error "No tokens to express"
parseOld (NumTok x:ts) = NumE x
parseOld (OpTok op:ts) = 
  let lt = parseOld ts
      rt = parseOld (drop (sizeExpr lt) ts)
  in OpE op lt rt


treeA, treeB, treeC, treeOne :: Expr
treeA = NumE 79
treeB = OpE Plus (NumE 79) (NumE 4)
treeC = OpE Times (NumE 8) (NumE 2)
treeOne = OpE Minus treeB treeC

type Value = Double

eval :: Expr -> Value
eval (NumE x) = x
eval (OpE Plus lt rt) = (eval lt) + (eval rt)
eval (OpE Minus lt rt) = (eval lt) - (eval rt)
eval (OpE Times lt rt) = (eval lt) * (eval rt)
eval (OpE Div lt rt) = (eval lt) / (eval rt)
 {-
eval (NumE x)= x
eval (OpE op lt rt) = 
  let ltVal = eval lt
      rtVal = eval rt
      opVal = case op of 
                Plus -> (+)
                Minus -> (-)
                Times -> (*)
                Div -> (/)
  in ltVal `opVal` rtVal

eval (OpE op lt rt) = (evalOp op) (eval lt) (eval rt)
  where evalOp Plus = (+)
        evalOp Minus = (-)
        evalOp Times = (*)
        evalOp Div = (/)
-}

rep :: String -> Maybe Value
rep str = 
  case lexer str of
    Nothing -> Nothing
    Just tokens -> applyToMaybe eval (parse tokens)

main = do
  str <- getLine
  case rep str of
    Nothing -> putStrLn "Invalid line, try again."
    Just val -> putStrLn $ show val
  main
