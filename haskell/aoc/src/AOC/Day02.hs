{-# LANGUAGE OverloadedStrings #-}

module AOC.Day02 where

import AOC.Utils (readItemsFromFileWith)
import Data.Monoid (Sum (..))
import qualified Data.Text as T

data PwdRule = PwdRule
  { minOccur :: Int,
    maxOccur :: Int,
    letter :: Char
  }
  deriving (Eq, Show)

data Entry = Entry
  { pwdRule :: PwdRule,
    pw :: T.Text
  }
  deriving (Eq, Show)

mkEntry :: Int -> Int -> Char -> T.Text -> Entry
mkEntry min max c = Entry (PwdRule min max c)

isValidEntry :: Entry -> Bool
isValidEntry (Entry (PwdRule min max c) str)
  | count <= max && count >= min = True
  | otherwise = False
  where
    count = T.length $ T.filter (== c) str

isValidEntry' :: Entry -> Bool
isValidEntry' (Entry (PwdRule f s c) str)
  | first && not second || not first && second = True
  | otherwise = False
  where
    first = T.unpack str !! (f -1) == c
    second = T.unpack str !! (s -1) == c

parseEntry :: T.Text -> Entry
parseEntry e = Entry pwdRule pw
  where
    splits = T.splitOn ": " e
    pwdRule = parsePwdRule (head splits)
    pw = splits !! 1

parsePwdRule :: T.Text -> PwdRule
parsePwdRule r = PwdRule min max letter
  where
    splits = T.splitOn " " r
    bounds = head splits
    min = (read . T.unpack) $ head (T.splitOn "-" bounds)
    max = (read . T.unpack) (T.splitOn "-" bounds !! 1)
    letter = T.head $ splits !! 1

solveWith :: (Entry -> Bool) -> FilePath -> IO ()
solveWith pred path = do
  contents <- readItemsFromFileWith path id
  print $ getSum $ foldMap (toSum . pred . parseEntry) contents
  where
    toSum x = if x then Sum 1 else Sum 0

solvePart1 :: FilePath -> IO ()
solvePart1 = solveWith isValidEntry

solvePart2 :: FilePath -> IO ()
solvePart2 = solveWith isValidEntry'
