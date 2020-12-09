module AOC.Day05 where


-- blatantly copied from https://github.com/bzuilhof/AdventOfCode/blob/master/2020/day5.hs
-- because i had absolutely 0 motivation to do this binary fiddling BS

import AOC.Utils
import qualified Data.Text as T
import qualified Data.List as L

getInputData :: FilePath -> IO [String]
getInputData p = readItemsFromFileWith p T.unpack

getSeadId :: String -> Int
getSeadId input = getRow (take 7 input) * 8 + getColumn (drop 7 input)

getRow, getColumn :: String -> Int
getRow input = binarySearch input 0 127
getColumn input = binarySearch input 0 7

binarySearch :: String -> Int -> Int -> Int
binarySearch [x] lb ub
  | x == 'F' || x == 'L' = lb
  | otherwise = ub
binarySearch (x : xs) lb ub
  | x == 'F' || x == 'L' = binarySearch xs lb (ub - halfSize)
  | otherwise = binarySearch xs (lb + halfSize) ub
  where
    halfSize = getHalfSize lb ub

getHalfSize :: Integral a => a -> a -> a
getHalfSize lb ub = (ub - lb) `div` 2 + 1

findHole :: (Eq a, Num a) => [a] -> a
findHole (x : y : xs)
  | y - 2 == x = x + 1
  | otherwise = findHole (y : xs)

solvePart1 :: FilePath -> IO ()
solvePart1 p = do
  input <- getInputData p
  let seatId = maximum $ getSeadId <$> input
  putStrLn $ "max seatId is " ++ show seatId

solvePart2 :: FilePath -> IO ()
solvePart2 p = do
  input <- getInputData p
  let hole = findHole $ L.sort $ getSeadId <$> input
  putStrLn $ "hole found: " ++ show hole
