data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n    ) = show n
  show (App o l r) = brak l ++ show o ++ brak r
   where
    brak (Val n) = show n
    brak e       = "(" ++ show e ++ ")"

-- subs [1..3] == [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
subs :: [a] -> [[a]]
subs []       = [[]]
subs (x : xs) = yss ++ map (x :) yss where yss = subs xs

-- interleave 1 [2..4] == [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
interleave :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

-- perms
perms :: [a] -> [[a]]
perms []       = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))
