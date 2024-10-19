run :: String -> Either String Float
run  x =  evaluate (parse(tokenize x))

tokenize :: String -> [String]
tokenize = words

data Action = Add|Sub|Mult|Div|Take Float|Dub|Kill|Second|Third deriving Show

parse :: [String] -> [Action]
parse = map p2
    where
        p2 "+" = Add
        p2 "-" = Sub
        p2 "*" = Mult
        p2 "/" = Div
        p2 "d" = Dub
        p2 "k" = Kill
        p2 "s" = Second
        p2 "t" = Third
        p2 n = Take (read n)


evaluate :: [Action] -> Either String Float
evaluate = stack []
    where
        stack [answer] [] = Right answer
        stack xs (Take n:ys) = stack (n:xs) ys
        stack (x:y:zs) (Add:rest) = stack (y + x:zs) rest
        stack (x:[]) (Sub:rest) = stack (-x:[]) rest
        stack (x:y:zs) (Sub:rest) = stack (y - x:zs) rest
        stack (x:y:zs) (Mult:rest) = stack (x*y:zs) rest
        stack (x:y:zs) (Div:rest) = stack (y/x:zs) rest
        stack (x:xs) (Dub:ys) = stack (x:x:xs) ys
        stack (x:xs) (Kill:ys) = stack xs ys
        stack (x:y:xs) (Second:ys) = stack (y:x:xs) ys
        stack (x:y:z:xs) (Third:ys) = stack (z:x:y:xs) ys
        stack (x:[]) _ = error "Exception: Type Error"
        stack _ _ = error "Exception: Syntax Error"

