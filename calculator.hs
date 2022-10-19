---- cannot inspect values
--data Token = Number Double | Oper (Double -> Double -> Double) deriving Show  
---- lacks fidelity
--data Token = Number Double | Oper Char deriving Show

--Valid but we voted against it
--data Token = NumberTok Double | PlusTok | MinusTok | TimesTok | DivTok deriving Show

data Token = Number Double | Op Operator deriving Show
data Operator = Plus | Minus | Times | Div deriving Show

data Expr = Undefined
type Value = Double

lex :: String -> [Token]
lex str = map lexWord (words str)
  where lexWord :: String -> Token
        

parse :: [Token] -> Expr
parse = undefined

eval :: Expr -> Value
eval = undefined
