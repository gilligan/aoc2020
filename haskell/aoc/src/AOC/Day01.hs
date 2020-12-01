module AOC.Day01
  ( solvePart1,
    solvePart2,
    findSumsPart1,
    findSumsPart2,
  )
where

import AOC.Utils (readItemsFromFile)

findSumsPart1 :: [Int] -> Int
findSumsPart1 xs = head [(x * y) | x <- xs, y <- xs, x + y == 2020]

findSumsPart2 :: [Int] -> Int
findSumsPart2 xs = head [(x * y * z) | x <- xs, y <- xs, z <- xs, z + x + y == 2020]

solvePart1 :: FilePath -> IO ()
solvePart1 p = readItemsFromFile p >>= print . findSumsPart1

solvePart2 :: FilePath -> IO ()
solvePart2 p = readItemsFromFile p >>= print . findSumsPart2
