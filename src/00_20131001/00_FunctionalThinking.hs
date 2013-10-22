import Data.List
import Data.Char

------------
---- Main
------------

main = do
    putStrLn "input IP address"
    input <- getLine
    putStrLn ("Here is your input: " ++ input)

------------
--  p-Adic Number
------------

type AdicNumber = (Int, [Int])
getBase (base, digits) = base
getDigits (base, digits) = digits

toAdicNumber :: Int -> Int -> AdicNumber
toAdicNumber p decimal = (p, reverse $ unfoldr (expand p) decimal)
    where
        expand :: Int -> Int -> Maybe (Int, Int)
        expand p 0 = Nothing
        expand p n = Just (n `mod` p, n `div` p)

toDecimal :: AdicNumber -> Int
toDecimal (base, digits) = innerProduct bases digits
    where
        bases = reverse [ base^a | a <- take (length digits) [0..] ]
        innerProduct xs ys = sum $ zipWith (*) xs ys

------------
--  Binary
------------

type Binary = [Int]
toBinary :: Int -> Binary
toBinary num = getDigits $ toAdicNumber 2 num

type FixedBinary = [Int]
toFixedBinary :: Int -> Int -> FixedBinary
toFixedBinary n = fixList n 0 . toBinary

------------
--  Address
------------

data Address = IPAddress | SubnetMask deriving (Show)
-- toAddress :: String -> Address
-- toAddress = join . map toFixedBinary 8 . split "."

--- type IPAddress      = (NetworkAddress, HostAddress)
--- type NetworkAddress = FixedBinary
--- type HostAddress    = FixedBinary
type IPAddress  = FixedBinary
type SubnetMask = FixedBinary
type Prefix     = Int

getNetworkAddress :: IPAddress -> SubnetMask -> FixedBinary
getNetworkAddress = zipWith binAnd

getHostAddress :: IPAddress -> SubnetMask -> FixedBinary
getHostAddress = zipWith binOr

binOr :: Int -> Int -> Int
binOr 0 0 = 0
binOr _ _ = 1

binAnd :: Int -> Int -> Int
binAnd 1 1 = 1
binAnd _ _ = 0

binXor :: Int -> Int -> Int
binXor x y
    | x == y = 1
    | x /= y = 0

binNot :: Int -> Int
binNot 0 = 1
binNot 1 = 0

-- printAddress :: Address -> String
-- nth, join, split

-- isGlobal :: IPAddress -> Bool
-- isPrivate :: IPAddress -> Bool
-- isLoopBack :: IPAddress -> Bool
-- isBroadCast :: IPAddress -> Bool
-- 
-- getClass :: IPAddress -> Char
-- getHostAddress :: IPAddress -> SubnetMask -> HostAddress
-- getNetworkAddress :: IPAddress -> SubnetMask -> HostAddress
-- getNetworkAddress :: IPAdress -> SubnetMask -> 

------------
--  Pretty Print
------------

-- toPrefix :: SubnetMask -> Int
-- toPrefix = sum
-- -- toPrefix mask = length $ filter (==1) mask
-- 
-- toSubnetMask :: Int -> SubnetMask
-- toSubnetMask prefix = getDigits $ toAdicNumber 2 prefix

------------
--  Utility of List
------------

fixList :: Int -> a -> [a] -> [a]
fixList n x xs
    | n > length xs = iter n (x:) xs
    | otherwise     = take n xs

------------
--  Utility of List
------------

splitBy :: Char -> String -> [String]
splitBy c [] = []
splitBy c xs = takeWhile (/= c) xs : splitBy c rest
    where
        rest = drop 1 $ dropWhile (/= c) xs

stringToInt :: String -> Int
stringToInt str = toDecimal (10, digits)
    where
        digits = map digitToInt str

intToString :: Int -> String
intToString n = map intToDigit digits
    where
        digits = getDigits $ toAdicNumber 10 n

------------
--  Utility of Int
------------

iter :: Int -> (a -> a) -> a -> a
iter n f x = foldN f x n

foldN :: (a -> a) -> a -> Int -> a
foldN f x 0     = x
foldN f x n = f $ foldN f x (n-1)

--  MEMO

{-
data FixedBinary Int = [Int]
type AddressV4 = FixedBinary 8*4
type AddressV6 = FixedBinary 8*6

Formalize :: Binary -> FixedBianry

data Address = IPAddress | SubnetMask


parar :: (a -> ([a], b) -> b) -> b -> [a] -> b
parar f e [] = e
parar f e (x:xs) = f x (xs, parar f e xs)

genericIndexes :: [a] -> [Int]
genericIndexes []   = []
genericIndexes xs = (length xs) : (genericIndexes $ tail xs)

binaryToDecimal :: String -> String
binaryToDecimal = 
decimalToBinary
hexadecimal


toAdicNumber p decimal = (p, expand p decimal)
    where
        expand :: Int -> Int -> [Digit]
        expand p 0 = []
        expand p n = (n `mod` p) : expand p (n `div` p)
-}
