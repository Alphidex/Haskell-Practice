-- Exercise 1
times :: Num num=> [num] -> num
times [] = 1
times (x:xs) = x * times xs

-- Basically [x..y]
rng :: Int -> Int -> [Int]
rng x y
    | y-x+1 == 0 = []
    | otherwise = x : rng (x+1) y

factorial :: Int -> Int -- How do I change the values to big Int so that it works on larger numbers. Also Whats is type declarion used for?
-- And what is Num a => used for
factorial x = times (rng 1 x)

-- Exercise 2
count :: [Int] -> Int
count [] = 0
count (x:xs) = (1+) (count xs)

append :: Num a => [a] -> [a] -> [a]
append (x:xs) [] = x:xs
append [] (y:ys) = (y:ys)
append (x:xs) (y:ys) = x : append xs (y:ys)

-- (x:xs) = [[a, b] [c,d]] = [(x:xs), xss]
concatenate :: [[Int]] -> [Int]
concatenate [[]] = []
concatenate [(x:xs)] = (x:xs)
concatenate (x:xs) = x ++ concatenate xs

-- Exercises 3
member :: Eq eq => eq -> [eq] -> Bool
member _ [] = False
member x (y:ys) = x == y || member x ys

remove :: Eq eq => eq -> [eq] -> [eq]
remove _ [] = []
remove x (y:ys)
    | x==y = remove x ys
    | otherwise = y : remove x ys

at :: [Int] -> Int -> Int
at [] _ = error "Index is bigger than length of list"
at (x:xs) y
    | y<0 = error "Index is less than 0"
    | y==0 = x
    | otherwise = at xs (y-1)

-- Exercise 4
final :: [a] -> a
final [x] = x
final (x:xs) = final xs

ordered :: [Int] -> Bool
ordered [] = True
ordered [x] = True
ordered (x:y:xs) = (x<=y) && (ordered (y:xs))

pair :: [a] -> [b] -> [(a,b)]
pair [] _ = []
pair _ [] = []
pair (x:xs) (y:ys) = (x, y) : pair xs ys

find :: Eq num => num -> [(num, String)] -> String
find _ [] = ""
find x ((int, str):zs)
    | x==int = str
    | otherwise = find x zs

-- Exercise 5 Skip
-- Exercise 6 Merge Sort
merge :: [Int] -> [Int] -> [Int]
merge (x:xs) [] = (x:xs)
merge [] (y:ys) = (y:ys)
merge (x:xs) (y:ys)
    |x<=y = x:merge xs (y:ys)
    |otherwise = y:merge (x:xs) ys

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort (x:xs) = merge (msort first)  (msort second)
    where
        y = (x:xs)
        len = length y
        first = take (len `div` 2) y
        second = drop (len - (len `div` 2)) y

