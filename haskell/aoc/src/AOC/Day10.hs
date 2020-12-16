module AOC.Day10 where

import AOC.Utils (readItemsFromFile)
import Data.Functor ((<&>))
import qualified Data.List as L
import Data.Monoid
import qualified Data.Set as S

type Joltage = Int

findConnection :: Joltage -> [Joltage] -> [(Joltage, Joltage)]
findConnection _ [] = []
findConnection joltage adapters = (joltage, selected) : findConnection selected (remove selected adapters)
  where
    matching = L.filter (\a -> a `elem` [joltage .. joltage + 3]) adapters
    selected = head matching
    remove x xs = L.filter (/= x) xs

getDiffResult :: [(Joltage, Joltage)] -> Int
getDiffResult js = ones * threes
  where
    diffs = (\(a, b) -> abs $ a - b) <$> js
    ones = length $ filter (== 1) diffs
    threes = length $ filter (== 3) diffs

countPossibleConnections :: Joltage -> S.Set Joltage -> Sum Int
countPossibleConnections joltage adapters
  | null $ matching joltage adapters = 1
  | otherwise = foldMap go (matching joltage adapters)
  where
    matching j adapters = S.filter (`elem` [j .. j + 3]) adapters
    go j = countPossibleConnections j (S.delete j adapters)

solvePart1 :: FilePath -> IO ()
solvePart1 p = do
  input <- readItemsFromFile p <&> L.sort
  let connection = findConnection 0 (maximum input + 3 : input)
  print $ getDiffResult connection

solvePart2 :: FilePath -> IO ()
solvePart2 p = do
  x <- readItemsFromFile p <&> L.sort
  let input = L.sort $ maximum x + 3 : x
  let count = countPossibleConnections 0 (S.fromList input)
  print $ getSum count

-- copy pasted from https://github.com/DestyNova/advent_of_code_2020/blob/main/day10/Part2.hs
-- as i was too stupid to figure out why my `countPossibleConnections` is wrong.
part2 :: FilePath -> IO ()
part2 p = do
  txt <- readFile p
  let nums = read <$> lines txt
  print $ 0 : L.sort nums ++ [maximum nums + 3]
  print $ jolts nums

jolts :: (Num a2, Num a1, Ord a1) => [a1] -> a2
jolts nums =
  let xs = 0 : L.sort nums ++ [maximum nums + 3]
   in product $ combos xs

combos :: (Ord a1, Num a1, Num a2) => [a1] -> [a2]
combos (a : b : c : d : e : xs)
  | e - a == 4 = 7 : combos (d : e : xs)
  | otherwise = ca : combos (b : c : d : e : xs)
  where
    ca = if c - a <= 3 then 2 else 1
    db = if d - b <= 3 then 2 else 1
combos _ = [1]
