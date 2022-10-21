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
tokensOne = [OpTok Minus, OpTok Plus, NumTok 79, NumTok 4, OpTok Times, NumTok 8, NumTok 2]


data Expr = NumE Double | OpE Operator Expr Expr deriving (Show, Eq)
parse :: [Token] -> Expr
parse = undefined

treeA, treeB, treeC, treeD :: Expr
treeA = NumE 79
treeB = OpE Plus (NumE 79) (NumE 4)
treeC = OpE Times (NumE 8) (NumE 2)
treeOne = OpE Minus treeB treeC

type Value = Double

eval :: Expr -> Value
eval (NumE x)= x
eval (OpE op lt rt) = undefined
