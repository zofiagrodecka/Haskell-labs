data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

flattenBT :: BinTree a -> [a]
flattenBT EmptyBT = []
flattenBT (NodeBT n lt rt) = [n] ++ flattenBT lt ++ flattenBT rt

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
insert a EmptyBT = NodeBT a EmptyBT EmptyBT
insert a (NodeBT n lt rt) | a <= n = NodeBT n (insert a lt) rt
                          | otherwise = NodeBT n lt (insert a rt)

list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)
list2BST [] = EmptyBT
list2BST [n] = NodeBT n EmptyBT EmptyBT
list2BST l = NodeBT (l !! half) (list2BST lt) (list2BST rt)
                where
                half = length l `div` 2
                lt = take half l
                rt = drop (half+1) l


data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a) |
              Subtract (Expr a) (Expr a) |
              Multiplicate (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Subtract m s) = eval m - eval s
eval (Multiplicate f1 f2) = eval f1 * eval f2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Subtract m s) = "(" ++ show' m ++ "-" ++ show' s ++ ")"
show' (Multiplicate f1 f2) = "(" ++ show' f1 ++ "*" ++ show' f2 ++ ")"
