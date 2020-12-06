module AOC.Day03 where

import AOC.Utils (readItemsFromFileWith)
import Data.Bifunctor
import qualified Data.Text as T

type Width = Int

type Height = Int

type WalkF = (Int, Int) -> (Int, Int)

getInput :: FilePath -> IO (Int, Int, [Int])
getInput p = do
  input <- readItemsFromFileWith p T.unpack
  return $ (length $ head input, length input, toInt <$> concat input)
  where
    toInt c = if c == '#' then 1 else 0

at :: (Int, Int) -> Width -> [Int] -> Int
at (x, y) width world = world !! ((y * width) + (x `mod` width))

walkWith :: Width -> Height -> [Int] -> WalkF -> Int
walkWith width height world f = go (0, 0) world
  where
    go :: (Int, Int) -> [Int] -> Int
    go (x, y) world
      | y >= height = 0
      | otherwise = curr + go (f (x, y)) world
      where
        curr = at (x, y) width world

solvePart1 :: FilePath -> IO ()
solvePart1 p = do
  (width, height, world) <- getInput p
  let trees = walkWith width height world f
  putStrLn $ "Encountered " ++ show trees ++ " trees"
  where
    f = bimap (+ 3) (+ 1)

solvePart2 :: FilePath -> IO ()
solvePart2 p = do
  (width, height, world) <- getInput p
  let walk = walkWith width height world
  print $
    product
      [ walk (bimap (+ 1) (+ 1)),
        walk (bimap (+ 3) (+ 1)),
        walk (bimap (+ 5) (+ 1)),
        walk (bimap (+ 7) (+ 1)),
        walk (bimap (+ 1) (+ 2))
      ]
