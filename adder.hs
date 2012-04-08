-- adder.hs
-- Adds all the numbers in a given file, formatted one number per line.
-- Comments are allowed and  are removed in `getNums'.
-- In truth, any line that doesn't start with a number is ignored.
-- BUG: Doesn't support Doubles. Need to regex the numbers! i.e. xxx.xxx

import System.Environment (getArgs)
import Data.Char (ord, isDigit)

main = do
  (filename:_) <- getArgs
  nums <- getNums filename
  print $ addUp nums

getNums :: String -> IO [String]
getNums filename = readFile filename >>= return . parseNums
      
parseNums :: String -> [String]
parseNums = filter goodNum . map (head . words) . filter (/= "") . lines
    where goodNum = and . map isDigit

addUp :: (Num a, Read a) => [String] -> a
addUp = foldl (\acc num -> acc + read num) 0 

   