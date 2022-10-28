import Debug.Trace

data Contest = Rock | Scissors | Paper deriving Show
data Velocity = MPS Double | FPS Double 

instance Show Velocity where
  show (FPS x) = (show x) ++ " ft/s"
  show (MPS x) = (show x) ++ " m/s"

instance Eq Velocity where
  (==) (FPS x) (FPS y) = (x == y)
  (==) (MPS x) (MPS y) = (x == y)
  (==) (FPS x) (MPS y) = (x/3.28 == y)
  (==) (MPS x) (FPS y) = (x == y/3.28)


type Point = (Double, Double)
data Shape = Circle Point Double | Rectangle Point Point deriving Show
data Operator = Add | Mult | Div | Sub deriving Show
data Expr = Number Float | Oper Operator Expr Expr deriving Show

instance Eq Shape where
  (==) (Circle center radius) (Circle center2 radius2) = (radius == radius2) && (center == center2)
  (==) (Rectangle (x1,y1) (x2, y2)) (Rectangle (w1,z1) (w2,z2)) = 
          (sort [x1, x2]) == (sort [w1, w2]) && (sort [y1, y2]) == (sort [z1, z2]) 

singleton x = Node Leaf x Leaf

aTree = Node (singleton 5) 7 (Node (singleton 10) 15 (singleton 16))

data BST a = Leaf | Node (BST a) a (BST a) deriving Show

instance Eq a => Eq (BST a) where
  (==) treeA treeB = (toList treeA) == (toList treeB)

--insert :: Ord a => a -> BST a -> BST a
insert y Leaf =  Node Leaf y Leaf
insert y tree@(Node left root right) = 
    case compare y root of
        EQ -> tree
        LT -> let newLeft =  insert y left
              in Node newLeft root right
        GT -> let newRight = insert y right
              in Node left root newRight

element :: Ord a => a -> BST a -> Bool
element x Leaf = False
element x (Node left root right) =
  case compare x root of
    EQ -> True
    LT -> element x left
    GT -> element x right
--element x (Node left root right) = (x == root) || (element x left) || (element x right)

toList :: BST a -> [a]
toList Leaf = []
toList (Node left root right) = (toList left) ++ (root:(toList right))

toTree :: Ord a => [a] -> BST a
{-toTree [] = Leaf
toTree (x:xs) = x `insert` (toTree xs)-}
toTree elems = foldr insert Leaf elems


treeB = toTree [1..10]
treeC = toTree [10,9..1]

checkEQ :: Eq a => BST a -> BST a -> Bool
checkEQ treeA treeB = (toList treeA) == (toList treeB)
