split :: Int -> [a] -> ([a],[a])
split n [] = ([],[])
split n (x:xs)
    | n <= 0 = ([] , x:xs)
    | otherwise = (x:ys , zs)
    where
        (ys,zs) = split (n-1) xs

size :: [a] -> Int
size = addSize 0
    where
        addSize n [] = n
        addSize n (_:xs) = addSize (n+1) (xs)
{-
factorial :: Num n => n -> n
factorial = factorialOne 1
    where
        factorialOne a 1 = 1
        factorialOne a b = factorialOne (a*b) (b-1)
-}
count :: Int -> [Int]
count = countDownOnto []
    where
        countDownOnto ns 0 = ns
        countDownOnto ns n = countDownOnto (n:ns) (n-1)
