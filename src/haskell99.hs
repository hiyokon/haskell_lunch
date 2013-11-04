import System.Random
import Control.Monad (replicateM)

-------- 21 to 28

------------ 21: insertAt

insertAt :: a -> [a] -> Int -> [a]
insertAt a xs     1 = a : xs
insertAt a (x:xs) n = x : insertAt a xs (n-1)

------------ 22: range

range :: Int -> Int -> [Int]
range m n
    | m <= n    = rangeHelper m (n-m+1)
    | otherwise = rangeHelper n (m-n+1)
    where
        rangeHelper k 0 = []
        rangeHelper k l = k : rangeHelper (k+1) (l-1)

-- range x y = take (y-x+1) $ iterate (+1) x

------------ 23: rndSelect

rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect xs n
    | n < 0 = error "N must be greater than zero."
    | otherwise = do
        pos <- replicateM n $ getStdRandom (randomR (0, (length xs)-1))
        return [xs !! p | p <- pos]

------------ 24: diffSelect

-- have to debug

diffSelect :: Int -> Int -> IO [Int]
diffSelect m n = diffSelectHelper m [1..n]

diffSelectHelper :: Int -> [a] -> IO [a]
diffSelectHelper 0 _  = return []
diffSelectHelper m xs = do
        pos     <- randomRIO (1, (length xs))
        let (r, ys) = removeAt pos xs
        rest <- diffSelectHelper (m-1) ys
        return (r : rest)

removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (l, x:r)
    where
        (l, r) = removeAt (n-1) xs

------------ 25: rndPermu

rndPermu :: [a] -> IO [a]
rndPermu xs = diffSelectHelper (length xs) xs

------------ 26: combinations

combinations :: Int -> [a] -> [[a]]
combinations 1 xs = [[x] | x <- xs]
combinations n xs = concatMap (\(r, rs) -> map (r:) (combinations (n-1) rs)) rests
        where
            rests = [removeAt m xs | m <- [1..(length xs)]]

------------ 27: group

-- debug

group :: Eq a => [Int] -> [a] -> [[[a]]]
group n xs = map init $ groupHelper n xs

groupHelper :: Eq a => [Int] -> [a] -> [[[a]]]
groupHelper []     xs = [[xs]]
groupHelper (n:ns) xs = concatMap (\cs -> map (cs:) (groupHelper ns (diff cs xs))) css
        where
            css  = combinations n xs

diff :: Eq a => [a] -> [a] -> [a]
diff _      [] = []
diff []     ys = ys
diff (x:xs) ys = diff xs (filter (/=x) ys)


------------ 28: lsort

lsort :: [[a]] -> [[a]]
lsort xss = map snd $ sortHelper [(length xs, xs) | xs <- xss]

lfsort :: [[a]] -> [[a]]
lfsort xss = map snd $ sortHelper (zip lfs xss)
    where
        ls  = map length xss
        lfs = map (\l -> count l ls) ls

count  :: Eq a => a -> [a] -> Int
count  a = foldr (\x -> if a == x then (+1) else id) 0

-- count :: Eq a => a -> [a] -> Int
-- count _ []       = 0
-- count a (x:xs)
--     | a == x    = 1 + count a xs
--     | otherwise = count a xs

sortHelper :: Ord l => [(l,a)] -> [(l,a)]
sortHelper []  = []
sortHelper (x:xs) =
    let smaller = sortHelper [ a | a <- xs, (fst a) <  (fst x)]
        bigger  = sortHelper [ a | a <- xs, (fst a) >= (fst x)]
    in smaller ++ [x] ++ bigger

-------- 31 to 41 : Arithmetic

------------ 31: isPrime

isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | n == 2    = True
    | otherwise = [] == [ x | x <- [3..(n-1)], n `mod` x == 0 ]

------------ 32: myGCD

myGCD :: Int -> Int -> Int
myGCD m n
    | n == 0    = m
    | am < an   = myGCD an am
    | otherwise = myGCD n (m `mod` n)
    where
        am = abs m
        an = abs n

------------ 33: coprime

coprime :: Int -> Int -> Bool
coprime m n = (myGCD m n) == 1

------------ 34: totient

totient :: Int -> Int
totient 1 = 1
totient n = length [ x | x <- [1..(n-1)], coprime n x ]

------------ 35: primeFactors

primeFactors :: Int -> [Int]
primeFactors n
    | n == 1    = []
    | otherwise = d : primeFactors (n `div` d)
    where
        d = head [ x | x <- [2..n], n `mod` x == 0 ]

------------ 36: primeFactorsMult

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map switch . encode . primeFactors

encode [] = []
encode (x:xs) = (length $ x : takeWhile (==x) xs, x)
                 : encode (dropWhile (==x) xs)

switch :: (a, b) -> (b, a)
switch (a, b) = (b, a)

------------ 37: totient'

totient' :: Int -> Int
totient' n = phi (primeFactorsMult n)
    where
        phi :: [(Int, Int)] -> Int
        phi [] = 1
        phi ((p,m):xs) = ((p-1) * (p ^ (m-1))) * phi xs

------------ 39: primesR

primesR :: Int -> Int -> [Int]
primesR m n = [ x | x <- range m n, isPrime x ]

-- eratostenes

------------ 40: goldbach

goldbach :: Int -> (Int, Int)
goldbach n
    | n == 2 || odd n = error "Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers."
    | otherwise       = head [ (a, b) | a <- pr, b <- pr, a + b == n ]
        where pr = primesR 2 (n-2)

------------ 41: goldbachList

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList m n = map goldbach [ x | x <- range m n, even x, x > 2 ]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' m n l = filter (bigger l) $ goldbachList m n
    where
        bigger l (a,b) = a >= l && b >= l

-------- 46 to 50 : Logic and Codes

------------ 46: table

not' :: Bool -> Bool
not' True  = False
not' False = True

and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool

infixl 4 `or'`
infixl 6 `and'`
infixl 9 `not'`

and' True True   = True
and' _    _      = False

or'  False False = False
or'  _     _     = True

nand' a b = not' $ and' a b
nor'  a b = not' $ or'  a b

xor' True  False = True
xor' False True  = True
xor' _     _     = False

impl' a b = (not' a) `or'` b

equ' True  True  = True
equ' False False = True
equ' _     _     = False

-- table :: (Bool -> Bool -> Bool) -> String
-- table f = printBinary f [True, False]

------------ 48: tablen

tablen :: Int -> ([Bool] -> Bool) -> String
tablen n f = printBinary' n f [True, False]

printBinary' :: (Show a, Show b) => Int -> ([a] -> b) -> [a] -> String
printBinary' n f domain
    = concatMap (++ "\n") [ printBinaryInstance' f xs | xs <- domains ]
        where
            domains = production n domain

printBinaryInstance' :: (Show a, Show b) => ([a] -> b) -> [a] -> String
printBinaryInstance' f xs
    = foldr (\a b -> show a ++ " " ++ show b) (show (f xs)) xs

production :: Int -> [a] -> [[a]]
production n xs = productHelper [[]] n xs
    where
        productHelper acc 0 xs = acc
        productHelper acc n xs = productHelper [ x:ys | x <- xs, ys <- acc ] (n-1) xs

-- sequence [[],[]], replicate

------------ 49: gray

gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n = left ++ right
    where
        left  = map ('0':) $ gray (n-1)
        right = reverse $ map (('1':) . tail) $  left

-------- 54 to 60 : Binary Trees

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

------------ 55: cbalTree

-- height is log2 n (+1 Empty)
mkTree :: Int -> Tree Char
mkTree 1 = Empty
mkTree n = Branch 'x' (mkTree m) (mkTree m)
    where
        m = n `div` 2

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n-1) `quotRem` 2
             in [Branch 'x' left right | i <- [q..q+r]
                                       , left  <- cbalTree i
                                       , right <- cbalTree (n-i-1)]

--      replace :: Tree a -> [Tree a]
--      replace Empty = [singleton 'x']
--      replace (Branch 'x' Empty Empty)
--          = [Branch (singleton 'x') Empty
--            ,Branch Empty (singleton 'x')
--            ]
--      replace (Branch 'x' left Empty)
--          =
--      replace (Branch 'x' left Empty)
--          =

------------ 56: symmetric

treeEq :: Tree a -> Tree a -> Bool
treeEq Empty Empty                   = True
treeEq (Branch _ a b) (Branch _ x y) = (treeEq a x) && (treeEq b y)
treeEq _              _              = False

mirror :: Tree a -> Tree a -> Bool
mirror Empty          Empty          = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _              _              = False

symmetric :: Tree a -> Bool
symmetric Empty                 = True
symmetric (Branch _ left right) = mirror left right

------------ 57: construct

-- learn you a haskell : making our own types and typeclasses
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Empty = leaf x
treeInsert x (Branch y left right)
    | x == y = Branch y left right
    | x <  y = Branch y (treeInsert x left) right
    | x >  y = Branch y left (treeInsert x right)

treeElem :: Ord a => a -> Tree a -> Bool
treeElem x Empty = False
treeElem x (Branch y left right)
    | x == y = True
    | x <  y = treeElem x left
    | x >  y = treeElem x right
-- learn you a haskell : making our own types and typeclasses

construct :: Ord a => [a] -> Tree a
construct = foldl (flip treeInsert) Empty

------------ 58: symCbalTrees

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = [ tree | tree <- cbalTree n, symmetric tree]

------------ 59: hbalTree

-- height balanced tree
hbalTree :: a -> Int -> [Tree a]
hbalTree a 0 = [Empty]
hbalTree a 1 = [leaf a]
hbalTree a n = [Branch a l r | l <- hbalTree a (n-2) , r <- hbalTree a (n-1)] ++
               [Branch a l r | l <- hbalTree a (n-1) , r <- hbalTree a (n-2)] ++
               [Branch a l r | l <- hbalTree a (n-1) , r <- hbalTree a (n-1)]

------------ 60: hbalTreeNodes

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes a 0 = [Empty]
hbalTreeNodes a 1 = [leaf a]
hbalTreeNodes a n = concatMap (filter (\x -> n == countNodes x) . hbalTree a) heights
    where
        heights = range (minHeight n) (maxHeight n)

maxNodes h = 2 ^ h - 1
minNodes h = 2 * (h - 1)

maxHeight n = n `div` 2 + 1
minHeight n = floor $ logBase 2 (fromIntegral (n :: Int) + 1)

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ r l) = 1 + (countNodes l) + (countNodes r)

-------- 61 to 70 : Binary Trees, continue

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

------------ 61: countLeaves

countLeaves :: Tree a -> Int
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l     r    ) = countLeaves l + countLeaves r

------------ 61: leaves

leaves :: Tree a -> [a]
leaves Empty                  = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l     r    ) = leaves l ++ leaves r

------------ 62: internals

internals :: Tree a -> [a]
internals Empty                  = []
internals (Branch _ Empty Empty) = []
internals (Branch x l     r    ) = x : (internals l ++ internals r)

------------ 62: atLevel

atLevel :: Tree a -> Int -> [a]
atLevel Empty                  _ = []
atLevel (Branch x r     l    ) 1 = [x]
atLevel (Branch x r     l    ) n = atLevel r (n-1) ++ atLevel l (n-1)

------------ 63: completeBinaryTree

completeBinaryTree :: a -> Int -> [Tree a]
completeBinaryTree a 0 = [Empty]
completeBinaryTree a 1 = [leaf a]
completeBinaryTree a n = [Branch a left right | left <- lefts, right <- rights]
    where
        (q, r) = n `quotRem` 2
        lefts  = completeBinaryTree a (q+r)
        rights = completeBinaryTree a q

------------ 64: layout

type Pos = (Int, Int)

layout :: Tree a -> Tree (a, Pos)
layout tree = fst $ layoutHelper (1, 1) tree
    where
        layoutHelper (x, y) Empty          = (Empty, x)
        layoutHelper (x, y) (Branch a l r) = (Branch (a, (x', y)) l' r', x'')
            where
                (l', x')  = layoutHelper (x   , y+1) l
                (r', x'') = layoutHelper (x'+1, y+1) r

------------ 65: layout

layout' :: Tree a -> Tree (a, Pos)
layout' tree = fst $ layoutHelper (1, 1) tree
    where
        layoutHelper (x, y) Empty          = (Empty, x)
        layoutHelper (x, y) (Branch a l r) = (Branch (a, (x', y)) l' r', x'')
            where
                (l', x')  = layoutHelper (x   , y+1) l
                (r', x'') = layoutHelper (x'+1, y+1) r











