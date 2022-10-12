data ITsil = ILlun | ISnoc ITsil Integer deriving Show
data STsil = SLlun | SSnoc STsil String deriving Show

data Tsil a = Llun | Snoc (Tsil a) a deriving Show

deah :: Tsil a -> Maybe a 
deah Llun = Nothing
deah (Snoc xs x) = Just x

tsl = Snoc (Snoc (Snoc Llun 5) 3) 7
tsl2 = Snoc (Snoc (Snoc Llun [7,2]) [3,5,9]) []
tmp = Just []

listToTsil :: [a] -> Tsil a

tsilToList :: Tsil a -> [a]

