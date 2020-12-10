module AOC.Day06 where

import AOC.Utils (readItemsFromFileWith)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.List.Split as S
import qualified Data.Text as T

type CharMap = M.Map Char Int

emptyMap :: CharMap
emptyMap = M.fromList $ zip ['a'..'z'] (repeat 0)

getInput :: FilePath -> IO [T.Text]
getInput p = do
  input <- readItemsFromFileWith p id
  return $ mconcat <$> split input
  where
    split = S.splitWhen (== "")


getInput' :: FilePath -> IO [[String]]
getInput' p = do
  input <- readItemsFromFileWith p T.unpack
  return $ split input
  where
    split = S.splitWhen (== "")

solvePart1 :: FilePath -> IO ()
solvePart1 p = do
    input <- getInput p
    let s = sum $ length . L.group . L.sort . T.unpack <$> input
    putStrLn $ "The sum is " ++ show s

solvePart2 :: FilePath -> IO ()
solvePart2 p = do
    input <- getInput' p
    let s = sum $ length . foldl1 L.intersect <$> input
    putStrLn $ "The sum is " ++ show s
