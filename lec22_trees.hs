import Debug.Trace

data Contest = Rock | Scissors | Paper deriving Show
data Velocity = MetersPerSecond Double | FeetPerSecond Double deriving Show
type Point = (Double, Double)
data Shape = Circle Point Double | Rectangle Point Point deriving Show
data Operator = Add | Mult | Div | Sub deriving Show
data Expr = Number Float | Oper Operator Expr Expr deriving Show


singleton x = Node Leaf x Leaf

aTree = Node (singleton 5) 7 (Node (singleton 10) 15 (singleton 16))

data BST a = Leaf | Node (BST a) a (BST a) deriving Show

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


toList :: BST a -> [a]
toList Leaf = []
toList (Node left root right) = (toList left) ++ (root:(toList right))

--toTree :: Ord a => [a] -> BST a
toTree [] = Leaf
toTree (x:xs) = x `insert` (toTree xs)


treeB = toTree [1..10]
treeC = toTree [10,9..1]

