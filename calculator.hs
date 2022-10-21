import Debug.Trace
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
lexer :: String -> [Token]
lexer str = map lexWord (words str)
  where lexWord :: String -> Token
        lexWord "+" = OpTok Plus
        lexWord "-" = OpTok Minus
        lexWord "/" = OpTok Div
        lexWord "*" = OpTok Times
        lexWord str = if all (`elem` ('.':['0'..'9'])) str
                      then NumTok (read str)
                      else error ("Invalid input in lexer: " ++ str)

tokensA = [NumTok 79]
tokensB = [OpTok Plus, NumTok 79, NumTok 4]
tokensW = [OpTok Minus, NumTok 4, OpTok Times, NumTok 8, NumTok 2]
tokensV = [OpTok Minus, OpTok Times, NumTok 8, NumTok 2, NumTok 4]
tokensOne = [OpTok Minus, OpTok Plus, NumTok 79, NumTok 4, OpTok Times, NumTok 8, NumTok 2]

sizeExpr :: Expr -> Int
sizeExpr (NumE _) = 1
sizeExpr (OpE op lt rt) = 1 + sizeExpr lt + sizeExpr rt

data Expr = NumE Double | OpE Operator Expr Expr deriving (Show, Eq)
parse :: [Token] -> Expr
parse []     = error "No tokens to express"
parse (NumTok x:ts) = NumE x
parse (OpTok op:ts) = 
  let lt = parse ts
      rt = parse (drop (sizeExpr lt) ts)
  in OpE op lt rt

parse2 toks = (aux toks)
  where aux :: [Token] -> (Expr, [Token])
        aux []     = error "No tokens to express"
        aux (NumTok x:ts) = (NumE x, ts)
        aux (OpTok op:ts) = 
          let (lt, afterLeft) = aux ts
              (rt, afterRight) = aux afterLeft
          in (OpE op lt rt, afterRight)

treeA, treeB, treeC, treeOne :: Expr
treeA = NumE 79
treeB = OpE Plus (NumE 79) (NumE 4)
treeC = OpE Times (NumE 8) (NumE 2)
treeOne = OpE Minus treeB treeC

type Value = Double

eval :: Expr -> Value

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
