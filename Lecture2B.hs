countdown :: Int -> [Int]
countdown n
    | n <=0 = []
    | otherwise = n : countdown(n-1)

isEmpty :: [a] -> Bool
isEmpty []    = True
isEmpty (x:xs) = False

hd :: [a] -> a
hd []   = error "Empty"
hd (x:_) = x


tl :: [a] -> [a]
tl [] = error "Empty"
tl (_:xs) = xs

addA :: [Int] -> Int
addA []  = 0
addA (x: xs) = x + addA xs

snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y : snoc x ys
{-
snoc 5 [3,4]
snoc 5 (3: [4]) = 3 : snoc  5 [4] => 3 : 4 : [5]
snoc 5 (4: []) = 4 : snoc 5 [] == [5]

-}

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = snoc x (rev xs)

{-
rev [3,4,5] = snoc 3 (rev [4,5]) = snoc 3 [5, 4] = [5,4,3]
rev [4, 5] = snoc 4 (rev [5]) => snoc 4 [5] = [5, 4]
rev [5] = snoc 5 []

-}

grab :: Int -> [a] -> [a]
grab n [] = []
grab n (x:xs)
    | n<=0 = []
    | otherwise = x : grab (n-1) xs

{-
grab 2 [1,2,3,4]
1 : grab(1) [2,3,4]
grab 1 [2,3,4]
2 : grab 0 [3,4] => 2 : []
grab 0 [3,4]
[]
-}

cntUp :: Int -> [Int]
cntUp n = n : cntUp (n+1)

{-
e.g. grab 5 (cntUp 5) = [5,6,7,8,9]

-}
