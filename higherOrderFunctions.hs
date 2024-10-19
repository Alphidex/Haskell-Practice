-- map odd [1..10] - List Comprehension
-- map ( $ 100) (map (+) [1..10]) = [101,102,103,104,105,106,107,108,109,110]
-- zipWith (+) [1..10] [10, 20..100] = [11,22,33,44,55,66,77,88,99,110]
-- 10 `mod` 3
-- (+6) 7 = 13
-- (6+) 7 = 13
-- (`mod` 3) 10 = 1
-- map (`mod` 3) [1..10] = [1,2,0,1,2,0,1,2,0,1]
-- (max 5) 10 = 10
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- curry :: ((a, b) -> c) -> a -> b -> c
-- map (uncurry (+)) [(1,2),(3,4),(5,6)] = [3,7,11]

add :: (Int, Int) -> Int
add (x,y) = x+y

--ghci> :t curry
--curry :: ((a, b) -> c) -> a -> b -> c
--ghci> :t add
--add :: (Int, Int) -> Int
--ghci> :t curry add
--curry add :: Int -> Int -> Int
--ghci> :t curry add 1
--curry add 1 :: Int -> Int
--ghci> :t (curry add) 1
--(curry add) 1 :: Int -> Int
--ghci> :t (cury add) 1 2
--ghci> :t (curry add) 1 2
--(curry add) 1 2 :: Int
--ghci> (curry add) 1 2
--3

currie :: ((a,b)->c) -> a->b->c
currie f x y = f (x,y)

uncurrie :: (a->b->c) -> (a,b) -> c
uncurrie f (x,y) = f x y

flyp :: (a -> b -> c) -> b -> a -> c
flyp f y x = f x y

-- For loop
iter :: Int -> (a -> a) -> a -> a
iter n f x
    | n <= 0 = x
    | otherwise = f (iter(n-1) f x)

    {-
    iter 1 (+2) 4
    == +2 (iter 0 +2 4)
    (iter 0 +2 4) -> n <= 0 = 4
    == +2 (4) = 6

    itr 2 (+2) 4
    == +2 (iter 1 +2 4)
    iter 1 +2 4 = +2 (iter 0 +2 4)
    == 8
    -}


-- map (iter 10 (+6)) [1..10]== [61,62,63,64,65,66,67,68,69,70]

-- While loop - c = condition
while :: (a -> Bool) -> (a -> a) -> a -> a
while c f x
    | c x = while c f (f x)
    | otherwise = x

    {-
    while <4 +2 1
    c x => <4 (1) = True == while (<4) +2 (+2 1)
    ==> while <4 +2 3 ==> while <4 +2 (5)
    ==> x ==> 5
    -}


