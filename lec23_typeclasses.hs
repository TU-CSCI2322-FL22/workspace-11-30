data Operator = Plus | Minus | Times | Div deriving (Show, Eq)
data Expr = NumE Double | OpE Operator Expr Expr deriving (Show, Eq)
data Contest = Rock | Scissor | Paper deriving Show
data Velocity = MPS Double | FPS Double deriving Show

type Point = (Double, Double)
data Shape = Circle Point Double | Rectangle Point Point deriving Show

data Tsil a = Llun | Snoc (Tsil a) a deriving Show
data BST a = Leaf | Node (BST a) a (BST a) deriving Show

class Bhoolish a where
    boo :: a -> Bool
    boo v = iffffy v True False

    iffffy :: a -> b -> b -> b
    iffffy x tCase fCase = if boo x then tCase else fCase

instance Bhoolish Bool where
  --boo :: Bool -> Bool
  --iffffy :: Bool -> b -> b -> b
  boo b = b
  iffffy True tCase fCase = tCase
  iffffy False tCase fCase = fCase
    
instance Bhoolish Int where
  iffffy x tCase fCase = if x /= 0 then tCase else fCase

instance Bhoolish [a] where
  boo lst = not (null lst)

spook :: Bhoolish a => [a] -> [a]
spook lst = [x | x <- lst, boo x]

data Token = NumTok Double | OpTok Operator deriving (Show, Eq)

instance Bhoolish Double where
  boo x = x > 0

instance Bhoolish Token where
  boo (NumTok x) = boo x
  boo (OpTok op) = op `elem` [Plus, Times]
  
instance Bhoolish Expr where
  boo (NumE x) = False
  boo (OpE op lft rgt) = boo (OpTok op) || boo lft || boo rgt
