-- adder.hs
-- Adds all the numbers in a given file, formatted one number per line.
-- Comments are allowed and  are removed in `parseNums`.
-- In truth, any line that doesn't start with a number is ignored.

import System.Environment (getArgs)
import Text.Regex.Posix ((=~))

main = do
  (fname:_) <- getArgs
  numbers   <- getNums fname
  print . sum . map readToDouble $ numbers
    where readToDouble n = read n :: Double

getNums :: String -> IO [String]
getNums fname = readFile fname >>= return . parseNums
      
parseNums :: String -> [String]
parseNums = filter goodNum . map (head . words) . filter (/= "") . lines
    where goodNum num = num =~ "^[0-9]+[.]?[0-9]+$" :: Bool
