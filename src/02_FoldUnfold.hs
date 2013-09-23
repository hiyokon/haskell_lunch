{-
 - The Fun of Programing Chapter 3
 -}

data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

wrap :: a -> List a
wrap x = Cons x Nil

nil :: List a -> Bool
nil Nil = True
nil (Cons x xs) = False

-- foldL {{{

foldL :: (a -> b -> b) -> b -> List a -> b
foldL f e Nil = e
foldL f e (Cons x xs) = f x (foldL f e xs)

---- Recurcive

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

append' :: [a] -> [a] -> [a]
append' []     ys = ys
append' (x:xs) ys = x : append' xs ys

concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = append' xs (concat' xss)

---- Higher Order
mapL :: (a -> b) -> List a -> List b
mapL f = foldL (\x ys -> Cons (f x) ys) Nil

appendL :: List a -> List a -> List a
appendL xs ys = foldL Cons ys xs

concatL :: List (List a) -> List a
concatL = foldL appendL Nil

---- data
list1 = Cons 2 (Cons 1 Nil)
list2 = Cons 4 (Cons 3 Nil)
list3 = Cons list1 (wrap list2)

-- foldL }}}

-- isort and paramorphism {{{

isort :: Ord a => List a -> List a
isort = foldL insert Nil

insert :: Ord a => a -> List a -> List a
insert y Nil = wrap y
insert y (Cons x xs)
    | y < x     = Cons y (Cons x xs)
    | otherwise = Cons x (Cons y xs)

paraL :: (a -> (List a, b) -> b) -> b -> List a -> b
paraL f e Nil         = e
paraL f e (Cons x xs) = f x (xs, paraL f e xs)

insertPar :: Ord a => a -> List a -> List a
insertPar y = paraL (swap y) (wrap y)
    where
        swap :: Ord a => a -> a -> (List a, List a) -> List a
        swap y x (xs, swp)
            | y < x     = Cons y (Cons x xs)
            | otherwise = Cons x swp

insertAna :: Ord a => a -> List a -> List a
insertAna y = unfoldL' (swap y)
    where
        swap :: Ord a => a -> List a -> Maybe (a, List a)
        swap y Nil = Nothing
        swap y (Cons x xs)
            | y < x     = Just (x, Cons y xs)
            | otherwise = Just (x, xs)

apoL' :: (b -> Maybe (a, Either b (List a))) -> b -> List a
apoL' f u = case f u of
                Nothing            -> Nil
                Just (x, Left v)   -> Cons x (apoL' f v)
                Just (x, Right xs) -> Cons x xs

insertApo :: Ord a => a -> List a -> List a
insertApo y = apoL' (swap y)
    where
        swap :: Ord a => a -> List a -> Maybe (a, Either (List a) (List a))
        swap y Nil = Nothing
        swap y (Cons x xs)
            | y < x     = Just (x, Left (Cons y xs))
            | otherwise = Just (x, Right xs)

-- isort and paramorphism }}}

-- ssort and unfold {{{

unfoldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a
unfoldL p f g b = if p b then Nil else Cons (f b) (unfoldL p f g (g b))

unfoldL' :: (b -> Maybe (a, b)) -> b -> List a
unfoldL' f u = case f u of
                    Nothing     -> Nil
                    Just (x, v) -> Cons x (unfoldL' f v)

---- anamorphisms
ssort :: Ord a => List a -> List a
ssort = unfoldL' delmin

delmin :: Ord a => List a -> Maybe (a, List a)
delmin = paraL step Nothing
    where
        step :: Ord a => a -> (List a, Maybe (a, List a)) -> Maybe (a, List a)
        step x (Nil, Nothing) = Just (x, Nil)
        step x (xs, Just (y, ys))
            | x < y     = Just (x, xs)
            | otherwise = Just (y, Cons x ys)

---- catamorphisms
minimumL :: Ord a => List a -> a
minimumL (Cons x xs) = foldL min x xs

---- paramorphisms
deleteL :: Eq a => a -> List a -> List a
deleteL y = paraL (f y) Nil
    where
        f :: Eq a => a -> a -> (List a, List a) -> List a
        f y x (xs, pfs)
            | y == x    = xs
            | otherwise = Cons x pfs

---- recursive
delmin' :: Ord a => List a -> Maybe (a, List a)
delmin' Nil = Nothing
delmin' xs  = Just (y, deleteL y xs)
    where
        y = minimumL xs

deleteL' :: Eq a => a -> List a -> List a
deleteL' y Nil = Nil
deleteL' y (Cons x xs)
    | y == x    = xs
    | otherwise = Cons x (deleteL y xs)

-- ssort and unfold }}}

-- bsort and apomorphism {{{

bsort :: Ord a => List a -> List a
bsort = unfoldL' bubble

bubble :: Ord a => List a -> Maybe (a, List a)
bubble = foldL step Nothing
    where
        step x Nothing = Just (x, Nil)
        step x (Just (y, ys))
            | x < y     = Just (x, Cons y ys)
            | otherwise = Just (y, Cons x ys)

-- bsort and apomorphism }}}

-- hylomorphism {{{
-- hylomorphism }}}

-- References {{{

{-

foldL :: (a -> b -> b) -> b -> List a -> b
foldL f e Nil = e
foldL f e (Cons x xs) = f x (foldL f e xs)

unfoldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a
unfoldL p f g b = if p b then Nil else Cons (f b) (unfold p f g (g b))

foldL' :: (Maybe (a, b) -> b) -> List a -> b
foldL' f Nil = f Nothing
foldL' f (Cons x xs) = f (Just (x, foldL' f xs))

unfoldL' :: (b -> Maybe (a, b)) -> b -> List a
unfoldL' f u = case f u of
                    Nothing     -> Nil
                    Just (x, v) -> Cons x (unfoldL' f v)

paraL :: (a -> (List a, b) -> b) -> b -> List a -> b
paraL f e Nil         = e
paraL f e (Cons x xs) = f x (xs, paraL f e xs)

ssort :: Ord a => List a -> List a
ssort = unfoldL' delmin

delmin :: Ord a => List a -> Maybe (a, List a)
delmin = paraL step Nothing
    where
        step :: Ord a => a -> (List a, Maybe (a, List a)) -> Maybe (a, List a)
        step x (Nil, Nothing) = Just (x, Nil)
        step x (xs, Just (y, ys))
            | x < y     = Just (x, xs)
            | otherwise = Just (y, Cons x ys)

bsort :: Ord a => List a -> List a
bsort = unfoldL' bubble

bubble :: Ord a => List a -> Maybe (a, List a)
bubble = foldL step Nothing
    where
        step :: Ord a => a -> Maybe (a, List a) -> Maybe (a, List a)
        step x Nothing = Just (x, Nil)
        step x (Just (y, ys))
            | x < y     = Just (x, Cons y ys)
            | otherwise = Just (y, Cons x ys)

-}

-- References }}}
