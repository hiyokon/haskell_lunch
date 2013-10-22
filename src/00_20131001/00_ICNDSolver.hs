import Data.List
import Data.Char

--------------------------
--  DEFINITION OF TYPE
--------------------------

--------
-- AdicNumber
--------

type AdicNumber = (Int, [Int])

--------
-- Bit and Binary
--------

data Bit = O | I deriving (Show, Eq)
type Binary = [Bit]

--------
-- Address
--------

data Address    = IPAddress | SubnetMask deriving (Show, Eq)
type IPAddress  = [Bit]
type SubnetMask = [Bit]

-- type IPAddress  = (32, [Bit])
-- type SubnetMask = (32, [Bit])

-- type Byte = (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
-- data Address = IPAddress
--                  Byte Byte Byte Byte
--              | SubnetMask
--                  Byte Byte Byte Byte
--              deriving (Show)

-- data Address = IPAddress
--                  Bit Bit Bit Bit Bit Bit Bit Bit
--                  Bit Bit Bit Bit Bit Bit Bit Bit
--                  Bit Bit Bit Bit Bit Bit Bit Bit
--                  Bit Bit Bit Bit Bit Bit Bit Bit
--              | SubnetMask
--                  Bit Bit Bit Bit Bit Bit Bit Bit
--                  Bit Bit Bit Bit Bit Bit Bit Bit
--                  Bit Bit Bit Bit Bit Bit Bit Bit
--                  Bit Bit Bit Bit Bit Bit Bit Bit
--              deriving (Show)

--------------------------
--  DEFINITION OF FUNCTIONS
--------------------------

------------
--  p-Adic Number
------------

getBase :: AdicNumber -> Int
getBase (base, digits) = base

getDigits :: AdicNumber -> [Int]
getDigits (base, digits) = digits

intToAdicNumber :: Int -> Int -> AdicNumber
intToAdicNumber p decimal = (p, reverse $ unfoldr (expand p) decimal)
    where
        expand :: Int -> Int -> Maybe (Int, Int)
        expand p 0 = Nothing
        expand p n = Just (n `mod` p, n `div` p)

adicNumberToInt :: AdicNumber -> Int
adicNumberToInt (base, digits) = foldr (\x y -> x + base * y) 0 (reverse digits)

------------
--  String <-> Int
------------

stringToInt :: String -> Int
stringToInt str = adicNumberToInt (10, digits)
    where
        digits = map digitToInt str

intToString :: Int -> String
intToString n = map intToDigit digits
    where
        digits = getDigits $ intToAdicNumber 10 n

------------
--  Int <-> Bit
------------

bitToInt :: Bit -> Int
bitToInt O = 0
bitToInt I = 1

intToBit :: Int -> Bit
intToBit 0 = O
intToBit 1 = I

------------
--  Bit -> Bit
------------

bAND :: Bit -> Bit -> Bit
bAND I I = I
bAND _ _ = O

bOR  :: Bit -> Bit -> Bit
bOR O O = O
bOR _ _ = I

bXOR :: Bit -> Bit -> Bit
bXOR x y
    | x == y = O
    | x /= y = I

bNOT :: Bit -> Bit
bNOT O = I
bNOT I = O

------------
--  Int <-> Binary
------------

binaryToInt :: Binary -> Int
binaryToInt bits = adicNumberToInt (2, digits)
    where
        digits = map bitToInt bits

intToBinary :: Int -> Binary
intToBinary n = map intToBit digits
    where
        digits = getDigits $ intToAdicNumber 2 n

------------
--  String <-> Address
------------

readAddress :: String -> IPAddress
readAddress str = foldr (\xs acc -> xs ++ acc) [] fixedBinaries
    where
        fixedBinaries = map (make8 . intToBinary . stringToInt) $ splitBy '.' str

showAddress :: IPAddress -> String
showAddress = joinWith '.' . map (intToString . binaryToInt) . chop8

subnetMaskToPrefix :: SubnetMask -> Int
subnetMaskToPrefix = sum . map bitToInt

prefixToSubnetMask :: Int -> SubnetMask
prefixToSubnetMask  n = take n (repeat I) ++ take (32-n) (repeat O)

------------
--  Address -> Address
--  Address -> Bool
------------

getNetworkAddress :: IPAddress -> SubnetMask -> IPAddress
getNetworkAddress = zipWith bAND

isBelongToSameNetwork :: IPAddress -> IPAddress -> SubnetMask -> Bool
isBelongToSameNetwork ip1 ip2 mask = (nwAddr1 == nwAddr2)
    where
        nwAddr1 = getNetworkAddress ip1 mask
        nwAddr2 = getNetworkAddress ip2 mask

--------------------------
--  UTILITIES
--------------------------

------------
--  Utility of List
------------

alignList :: Int -> a -> [a] -> [a]
alignList n x xs
    | n >= length xs = iter (n - length xs) (x:) xs
    | otherwise     = take n xs

splitList :: Int -> [a] -> [[a]]
splitList 0 xs = [xs]
splitList _ [] = []
splitList n xs = take n xs : splitList n (drop n xs)

------------
--  Utility of String
------------

splitBy :: Char -> String -> [String]
splitBy c "" = []
splitBy c xs = takeWhile (/= c) xs : splitBy c rest
    where
        rest = drop 1 $ dropWhile (/= c) xs

joinWith :: Char -> [String] -> String
joinWith c xss = init $ joinWithGen c xss
    where
        joinWithGen c [] = ""
        joinWithGen c (str:strs) = str ++ c : joinWith c strs

------------
--  Utility of Int
------------

iter :: Int -> (a -> a) -> a -> a
iter n f x = foldN f x n

foldN :: (a -> a) -> a -> Int -> a
foldN f x 0     = x
foldN f x n = f $ foldN f x (n-1)

------------
--  Utility of Binary
------------

make8 :: Binary -> Binary
make8 = alignList 8 O

chop8 :: Binary -> [Binary]
chop8 = splitList 8

------------
--  Utility of Address
------------


--------------------------
--  MEMO
--------------------------

{--

adicNumberToInt' :: AdicNumber -> Int
adicNumberToInt' (base, digits) = innerProduct bases digits
    where
        bases = reverse [ base ^ a | a <- take (length digits) [0..] ]
        innerProduct xs ys = sum $ zipWith (*) xs ys

intToBinary' :: Int -> Binary
intToBinary' n = map intToBit (reverse $ intToBinaryGen n)
    where
        intToBinaryGen :: Int -> [Int]
        intToBinaryGen 0 = [0]
        intToBinaryGen 1 = [1]
        intToBinaryGen n = (n `mod` 2) : intToBinaryGen (n `div` 2)

binaryToInt' :: Binary -> Int
binaryToInt' bits = sum [ w * b | (w, b) <- zip weights digits]
    where
        weights = iterate (*2) 1
        digits  = map bitToInt bits

showAddress :: Address -> String
showAddress = map (intToString . binaryToInt) . unfoldr split
        where
             split :: Binary -> Maybe (Binary, Binary)
             split [] = Nothing
             split xs = Just $ splitAt 4 xs
--}

{--
-                                                               
-       IN       AdicNumber                                     
-                 ^      \                                      
-                /        v                                     
-     String -> Int -> Binary                                   
-         \               /                                        
-          v             /                                         
-        Address <-------                                      
-          |                                                    
-          v                                                    
-        Address --------                                       
-         /              \                                         
-        v                v                                        
-     String <- Int <- Binary                                   
-                ^        /                                     
-                 \      v                                      
-       OUT      AdicNumber                                     
-                                                               
--}
