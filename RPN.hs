run :: String -> Int
run input = evaluate (parse (tokenize input))


tokenize :: String -> [String]
tokenize = words

data Action = Add | Subtract | Multiply | Push Int deriving (Show)

parse :: [String] -> [Action]
parse = map parseAction
    where
        parseAction "+" = Add
        parseAction "-" = Subtract
        parseAction "*" = Multiply
        parseAction s   = Push (read s)

evaluate :: [Action] -> Int
evaluate = evalStack []
  where
    evalStack :: [Int] -> [Action] -> Int
    evalStack [result] [] = result
    evalStack stack (Push n : actions) = evalStack (n : stack) actions
    evalStack (x:y:stack) (Add : actions) = evalStack ((y + x) : stack) actions
    evalStack (x:y:stack) (Subtract : actions) = evalStack ((y - x) : stack) actions
    evalStack (x:y:stack) (Multiply : actions) = evalStack ((y * x) : stack) actions
    evalStack _ _ = error "Invalid RPN expression"


data Stack = Stack [Int] deriving Show

emptyStack :: Stack
emptyStack = Stack []

push :: Int -> Stack -> Stack
push x (Stack xs) = Stack (x:xs)

pull :: Stack -> (Int , Stack)
pull (Stack []) = error "Error : Pulling from an empty stack !"
pull (Stack (x:xs)) = (x, Stack xs)
