import System.IO
import Data.String
import Text.Regex.Posix
import Codec.Binary.UTF8.String

main :: IO ()
main = interact processData

processData :: String -> String
processData = unlines . helper . lines
    where
        helper :: [String] -> [String]
        helper []  = []
        helper [line] = [line]
        helper (currLine:(postLine:rest)) = if postLine =~ encodeString "キャラ
                                  then joinWords ++ helper rest
                                  else currWords ++ helper (postLine:rest)
            where
                joinWords = zipWith (++) (words currLine) (words postLine)
                currWords = words currLine



----                ----
---- memo memo memo ----
----                ----

--        input  <- openFile "input.txt"  ReadMode
--        output <- openFile "output.txt" WriteMode
--        inputStr <- hGetContents input
--        let result = processData inputStr
--        hPutStr output result
--        hClose input
--        hClose output


-- lines = wordsBy (=='\n')
-- words = wordsBy (=='\t') whitespace

-- ++ "\n" ++
-- @_|\\

