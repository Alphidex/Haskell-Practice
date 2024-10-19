data List a = Nil | Cons a (List a)
    deriving Show

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)
