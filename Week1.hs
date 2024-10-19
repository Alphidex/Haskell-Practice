square :: Int -> Int
square x = x ^ 2

pyth :: Int -> Int -> Int -> Bool
pyth x y z = (square x + square y) == square z

factorial :: Int -> Int
factorial x
    | x <= 1 = 1
    | otherwise = factorial(x-1) * x

power :: Int -> Int->Int
power x y
    |even y = (x^2)^(div y 2)
    |odd y = (x^2)^(div(y-1) 2)
    |otherwise = error "Not possible"

-- Euclid doesn't work - Irrelevant
euclid :: Int -> Int -> Int
euclid x y
    |x==y=x
    |(x<0) || (y<0) = error "One of the values is below 0"
    |x>y = if mod x y == 0 then y else euclid y x
    |otherwise = if mod y x ==0 then x else euclid y (x-y)

pow :: Int->Int->Int
pow x y =
    if even y
    then (x^2)^(div y 2)
    else (x^2)^(div(y-1) 2)

-- Can do else if
{-
if condition
then operation
else if condition
    then operation
else operation
-}

collatz :: Int->Int
collatz x
    |x<=0 = error "X is smaller or equal to 0"
    |x==1 = 0
    |even x = 1+ collatz (div x 2)
    |odd x = 1+ collatz (3*x+1)
