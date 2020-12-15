module AOC.Day09 where

import AOC.Utils
import qualified Data.List as L

findNumber :: ([Int], [Int]) -> Maybe Int
findNumber (_, []) = Nothing
findNumber (prelude, x : xs)
  | x `elem` sums = findNumber (tail prelude ++ [x], xs)
  | otherwise = Just x
  where
    sums = uncurry (+) <$> [(a, b) | a <- prelude, b <- prelude, a /= b]

findNums :: Int -> [Int] -> Maybe Int
findNums _ [] = Nothing
findNums searched xs = case L.findIndex (\x -> sum x == searched) inits of
  Just i -> Just $ sum [minimum $ inits !! i, maximum $ inits !! i]
  Nothing -> findNums searched (tail xs)
  where
    inits = L.inits xs

getInput :: FilePath -> IO [Int]
getInput = readItemsFromFile

solvePart1 :: FilePath -> Int -> IO ()
solvePart1 p preludeSize = do
  input <- getInput p
  print $ findNumber (L.splitAt preludeSize input)

solvePart2 :: FilePath -> Int -> IO ()
solvePart2 p num = do
  input <- getInput p
  print $ findNums num input
