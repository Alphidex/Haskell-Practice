{-
Type Classes:
Num
Int->Int->Int
Num a => a-> a -> a
Eq
Ord



-}

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x <= y = x : (y:ys)
    | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

map2 :: (a->b) ->[a]->[b]
map2 f [] = []
map2 f (x:xs) = f x : map2 f xs

pointless :: [Int] -> [Bool]
pointless = map f
    where
     f x = 3 * (max 0 x) <= 5

halve :: [a] -> ([a],[a])
halve xs = (take n xs , drop n xs)
    where n = length xs `div` 2

split :: Int -> [a] -> ([a],[a])
split n [] = ([],[])
split n (x:xs)
    | n <= 0 = ([] , x:xs)
    | otherwise = (x:ys , zs)
    where
      (ys,zs) = split (n-1) xs
{-
split 3 [1..10]
1) x:ys, zs = 1:ys, zs
(ys,zs) = split (2) xs
2) x:ys, zs = 2:[3], [4..10]
ys,zs = split (1) xs
3) x:ys, zs = [3], [4..10]
ys,zs = split (0) xs
4) ys, zs = [], [4..10]
-}
