import           Data.Char

double x = x + x

quadruple x = double (double x)

factorial n = product [1 .. n]

average ns = sum ns `div` length ns

a = b + c
 where
  b = 1
  c = 2

d = a * 2

n = a `div` length xs
 where
  a  = 10
  xs = [1, 2, 3, 4, 5]

abool :: Bool
abool = 1 < 2

bools :: [Bool]
bools = [True, False]

swap (x, y) = (y, x)

even n = n `mod` 2 == 0

splitAt n xs = (take n xs, drop n xs)

recip n = 1 / n

abs n = if n >= 0 then n else -n

abs2 n | n >= 0    = n
       | otherwise = -n

add x y = x + y

const x _ = x

odds n = map f [0 .. n - 1] where f x = 2 * x + 1

odds2 n = map (\x -> 2 * x + 1) [0 .. n - 1]

halve :: [a] -> Maybe ([a], [a])
halve xs | mod n 2 == 0 = Just $ Prelude.splitAt (quot n 2) xs
         | otherwise    = Nothing
  where n = length xs

third :: [a] -> Maybe a
third (_ : _ : x : _) = Just x
third _               = Nothing

third2 :: [a] -> a
third2 xs = head $ tail $ tail xs

safetail xs = if null xs then [] else tail xs

safetail2 (_ : xs) = xs
safetail2 _        = []

luhnDouble x = if res > 9 then res else x where res = 2 * x - 9

luhn a b c d =
  let total = sum (map luhnDouble [a, b, c] ++ [d]) in mod total 10 == 0

let2int c = ord c - ord 'a'

int2let n = chr (ord 'a' + n)

shift n c | isLower c = int2let (mod (let2int c + n) 26)
          | otherwise = c

encode n xs = [ shift n x | x <- xs ]

sum_squares n = sum [ x ^ 2 | x <- [1 .. n] ]

square :: Int -> [(Int, Int)]
square n = [ (x, y) | x <- [0 .. n], y <- [0 .. n], x /= y ]

replicate :: Int -> a -> [a]
replicate n a = [ a | x <- [1 .. n] ]

scalarproduct :: [Int] -> [Int] -> Maybe Int
scalarproduct xs ys
  | length xs == length ys = Just $ sum $ map (\(x, y) -> x * y) $ zip xs ys
  | otherwise              = Nothing

factors :: Int -> [Int]
factors n | n >= 1    = [ i | i <- [1 .. n], mod n i == 0 ]
          | otherwise = []

perfect :: Int -> Bool
perfect n = n == sum [ i | i <- factors n, i < n ]

perfects :: Int -> [Int]
perfects n = [ i | i <- [1 .. n], perfect i ]

fac :: Int -> Int
fac 0 = 1
fac n | n >= 1    = n * fac (n - 1)
      | otherwise = 0

fac_mb :: Int -> Maybe Int
fac_mb 0 = Just 1
fac_mb n
  | n >= 1 = case fac_mb (n - 1) of
    Just k  -> Just (n * k)
    Nothing -> Nothing
  | otherwise = Nothing

fac_mb2 :: Int -> Maybe Int
fac_mb2 0 = Just 1
fac_mb2 n | n >= 1    = fmap (* n) (fac_mb2 (n - 1))
          | otherwise = Nothing

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0     = n + sumdown (n - 1)
          | otherwise = 0

and2 :: [Bool] -> Bool
and2 []           = True
and2 (True  : xs) = and2 xs
and2 (False : _ ) = False

concat2 :: [[a]] -> [a]
concat2 []         = []
concat2 (xs : xss) = xs ++ concat2 xss

(!!!) :: [a] -> Int -> Maybe a
(!!!) [] n = Nothing
(!!!) (x : xs) n | n == 0    = Just x
                 | otherwise = (!!!) xs (n - 1)

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) | x <= y    = [x] ++ merge xs (y : ys)
                        | otherwise = [y] ++ merge (x : xs) ys

sum_rec :: Integral a => [a] -> a
sum_rec []       = 0
sum_rec (x : xs) = x + sum_rec xs

all2 :: (a -> Bool) -> [a] -> Bool
all2 f []       = True
all2 f (x : xs) = f x && all2 f xs

all3 p = and . map p

data Tree a = Leaf a | Node (Tree a) (Tree a)

numLeaves :: Tree a -> Int
numLeaves (Leaf _  ) = 1
numLeaves (Node l r) = numLeaves l + numLeaves r

balanced :: Tree a -> Bool
balanced (Leaf _  ) = True
balanced (Node l r) = numLeaves l == balanced r
