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


------------ 28: -- @fun

-- @fun :: [a] ->
-- @fun

-------- 31 to 41



-------- 31 to 41


